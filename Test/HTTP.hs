module Test.HTTP (httpTest, session, get, getJSON, withJSON, post, postForm, postJSON, assert, assertEq, assertParse, failTest, debug, Program, Session, Url) where

import Network.Curl hiding (curlGetString)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import GHC.Conc
import qualified Data.Aeson as Ae
import Data.Aeson.Types (Parser, parseEither)
import Safe (readMay)
import System.Environment
import System.Exit
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Lazy (fromStrict, toStrict)


type Program = ReaderT (TVar [Results]) IO

data SessionState = SessionState { sessionResults :: Results,
                                   sessionBaseUrl :: String,
                                   sessionCurl :: Curl }

type Session a = State.StateT SessionState (ErrorT String IO) a

type Results = [(String, Maybe String)]

type Url = String

-- | Run one or more test sessions. httpTest will exit when done, with
-- exit code 1 if there were failures
httpTest :: Program () -> IO ()
httpTest m = withCurlDo $ do
   resTV <- newTVarIO []
   runReaderT m resTV
   finalRes <- fmap concat $ readTVarIO resTV

   mapM_ (putStrLn . ppRes) finalRes

   if any (isJust . snd) finalRes
      then exitWith $ ExitFailure 1
      else exitWith $ ExitSuccess

ppRes (nm, Nothing) = "Pass: "++nm
ppRes (nm, Just reason) = "FAIL: "++nm++"; "++reason


-- | Define a single test session based on session name and base url
session :: String -- ^ Session name (used for logging failures)
        -> Url -- ^ Base URL
        -> Session () -- ^ the actions and assertions that define the session
        -> Program ()
session sessionName baseURL m = do
   c <- liftIO $ initialize
   let state0 = SessionState [] baseURL c
   liftIO $ setopts c [CurlCookieJar (sessionName++"_cookies"), 
                       CurlFollowLocation True]
   res <- liftIO $ runErrorT $ State.execStateT m state0
   case res of 
      Right (SessionState res _ _) -> do
         res_tv <- ask
         liftIO $ atomically $ do 
            others <- readTVar res_tv
            writeTVar res_tv $ others ++ [reverse res]
      Left err -> do
         res_tv <- ask
         liftIO $ atomically $ do 
            others <- readTVar res_tv
            writeTVar res_tv $ others ++ 
                               [[(sessionName, 
                                 Just $ sessionName ++ " session failure:" ++err)]]

-- | GET a web page as a String
get :: Url -- ^ URL
    -> Session String
get url = do
  (code, res) <- getRaw url
  when (code /= CurlOK) $ 
     failTest ("GET "++url) (show code++"\nResponse:\n"++res)
  return res

getRaw :: Url -> Session (CurlCode, String) 
getRaw url = do
  SessionState _ base c <-  State.get
  liftIO $ curlGetString c (base++url) [] 

-- | GET a JSON value
getJSON :: Ae.FromJSON a => 
           Url  -- ^ URL
           -> Session a
getJSON url = do
  str <- get url
  case Ae.eitherDecode' $ fromStrict $ encodeUtf8 $ T.pack str of
    Right x -> return x
    Left err -> throwError $  "GET "++url ++ " JSON decoding failure: "++ err


-- | perform an action with a JSON value from a GET
withJSON :: Ae.FromJSON a => 
           String  -- ^ URL
           -> (a -> Session ()) -- ^ action to perform on successfully decoded value
           -> Session ()
withJSON url mu = do
  str <- get url
  case Ae.eitherDecode' $ fromStrict $ encodeUtf8 $ T.pack str of
    Right x -> mu x
    Left err -> do failTest ("GET "++url) $ "JSON decoding failure: "++ err
                   return ()

-- | Post a form
postForm :: Url -- ^ URL
         -> [(String,String)]  -- ^ form fields
         -> Session String
postForm url fields = post url $  map (\(x,y) -> x ++ ('=':y)) fields

-- | Post a string body
post :: Url -> [String] -> Session String
post url body = do
  SessionState _ base c <-  State.get
  (code, res) <- liftIO $ curlPostString c (base++url) [] body
  when (code /= CurlOK) $ 
     failTest ("POST "++url) (show code++"\nResponse:\n"++res)
  return res

-- | Post a JSON value
postJSON :: (Ae.ToJSON a, Ae.FromJSON b) => Url -> a -> Session b
postJSON url x = do str <- post url [T.unpack $ decodeUtf8 $ toStrict $ Ae.encode x]
                    case Ae.eitherDecode' $ fromStrict $ encodeUtf8 $ T.pack str of
                      Right x -> return x
                      Left err -> throwError $  "POST "++url ++ " JSON decoding failure: "++ err




-- | make an assertion
assert :: String -- ^ assertion name (used for reporting failures
       -> Bool -- ^ Boolean of which we are asserting truth
       -> Session ()
assert assName True = passTest assName
assert assName False = failTest assName "fail" 

-- | assert equality, for better output messages
assertEq :: (Show a, Eq a) => String -- ^ assertion name (used for reporting failures
       -> a  -- ^ a value
       -> a  -- ^ what it is meant to be equal to
       -> Session ()
assertEq assName x y | x == y    = passTest assName
                     | otherwise = failTest assName $ "not equal: "++show x ++" /= "++show y

-- | make an assertion in the Parser monad, for use with JSON value
assertParse :: String      -- ^ assertion name (used for reporting failures
            -> Parser Bool -- ^ Boolean of which we are asserting truth
            -> Session ()
assertParse assName pb  =
  case parseEither (const pb) () of
    Left err -> failTest assName $ "parse failure: "++err
    Right True -> passTest assName
    Right False -> failTest assName "fail" 


-- | Output a string to stdout if @--verbose@ is in command line arguments
debug :: String -> Session ()
debug s = do
  args <- liftIO $ getArgs
  if "--verbose" `elem` args
     then liftIO $ putStrLn s
     else return ()

  

addTestResult p = 
  State.modify $ \s -> s { sessionResults = p : sessionResults s }

passTest tstNm = addTestResult (tstNm, Nothing)
failTest tstNm reason =  addTestResult (tstNm, Just reason)


{-------------------------------------------------------
                CURL AUXILIARY FUNCTIONS
 -------------------------------------------------------}

-- | Custom version of getString that uses an existing Curl to handle cookies
curlGetString :: Curl -> URLString
              -> [CurlOption]
              -> IO (CurlCode, String)
curlGetString h url opts = do
   ref <- newIORef []
   -- Note: later options may (and should, probably) override these defaults.
   setopt h (CurlPostFields [])
   setopt h (CurlPost False)
   setopt h (CurlFailOnError True)
   setDefaultSSLOpts h url
   setopt h (CurlURL url)
   setopt h (CurlWriteFunction (gatherOutput ref))
   mapM_ (setopt h) opts
   rc <- perform h
   lss <- readIORef ref
   return (rc, concat $ reverse lss)

-- | Custom version of curlPost that uses an existing Curl to handle cookies
-- and returns the result in a string.
curlPostString :: Curl -> URLString -> [CurlOption] -> [String] -> IO (CurlCode, String)
curlPostString h url opts body = do
  ref <- newIORef []
  -- Note: later options may (and should, probably) override these defaults.
  setopt h (CurlFollowLocation True)
  setopt h (CurlFailOnError True)
  setopt h (CurlPost True)
  setopt h (CurlPostFields body)
  setDefaultSSLOpts h url
  setopt h (CurlURL url)
  setopt h (CurlWriteFunction (gatherOutput ref))
  mapM_ (setopt h) opts
  rc <- perform h
  lss <- readIORef ref
  return (rc, concat $ reverse lss)

{-------------------------------------------------------
                       ENCODING
 -------------------------------------------------------}
-- | FIXME (iperez): I don't know how to obtain all the possible
-- transformations that can occur in the output. This should contain all HTML
-- and Unicode representations that can be used by the server but were not in
-- the original file.
decode :: String -> String
decode [] = []
decode ('\\':'u':a:b:c:d:xs)
 | isHexDigit a && isHexDigit b && isHexDigit c && isHexDigit d
 = chr (hexToInt [a,b,c,d]) : decode xs
decode ('\\':'n': xs)                = '\n' : decode xs
decode ('\\':'"': xs)                = '"'  : decode xs
decode ('&':'q':'u':'o':'t':';':xs)  = '"'  : decode xs
decode ('&':'g':'t':';':xs)          = '>'  : decode xs
decode ('&':'l':'t':';':xs)          = '<'  : decode xs
decode (x : xs)                      = x    : decode xs

hexToInt :: String -> Int
hexToInt []     = 0
hexToInt [n]    = digitToInt n
hexToInt (n:ns) = digitToInt n * 16 + hexToInt ns

{-------------------------------------------------------
                    CONCURRENCY AUX
 -------------------------------------------------------}

-- | fork a thread and call the supplied function when the thread is about to
-- terminate, with an exception or a returned value. The function is called
-- with asynchronous exceptions masked. 
-- NOTE: This is taken from Control.Concurrency because it's not present in
-- GHC 7.4
{-forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ Control.Exception.try (restore action) >>= and_then
-}
