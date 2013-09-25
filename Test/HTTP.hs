module Test.HTTP where

import Network.Curl hiding (curlGetString)
import Network.Browser
import Network.HTTP
import Network.URI    ( parseURI )

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
import Safe (readMay)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)


type ProgramM = ReaderT (TVar [Results]) IO

data SuiteState = SuiteState { suiteResults :: Results,
                               suiteBaseUrl :: String,
                               suiteCurl :: Curl }

--what we really want
--type SuiteM = EitherT String (State.StateT SuiteState IO)

type SuiteM = State.StateT SuiteState IO

type Results = [(String, Maybe String)]

httpTest :: ProgramM () -> IO ()
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


session :: String -> String -> SuiteM () ->  ProgramM ()
session suiteName baseURL m = do
   c <- liftIO $ initialize
   let state0 = SuiteState [] baseURL c
   liftIO $ setopts c [CurlCookieJar (suiteName++"_cookies"), 
                       CurlFollowLocation True]
   SuiteState res _ _ <- liftIO $ State.execStateT m state0
   res_tv <- ask
   liftIO $ atomically $ do 
       others <- readTVar res_tv
       writeTVar res_tv $ others ++ [res]

get :: String -> SuiteM String
get url = do
  (code, res) <- getRaw url
  when (code /= CurlOK) $ 
     failTest ("GET "++url) (show code)     
  return res

getRaw :: String -> SuiteM (CurlCode, String) 
getRaw url = do
  SuiteState _ base c <-  State.get
  liftIO $ curlGetString c (base++url) [] 

getJSON :: Ae.FromJSON a => String -> SuiteM a
getJSON url = do
  str <- get url
  let Just x = Ae.decode' $ fromStrict $ encodeUtf8 $ T.pack str
  return x

assert :: String -> Bool -> SuiteM ()
assert assName True = 
  passTest assName
assert assName False =
  failTest assName "fail" 

addTestResult p = 
  State.modify $ \s -> s { suiteResults = p : suiteResults s }

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
curlPostString :: Curl -> URLString -> [CurlOption] -> [(String, String)] -> IO (CurlCode, String)
curlPostString h url opts fields = do
  ref <- newIORef []
  -- Note: later options may (and should, probably) override these defaults.
  setopt h (CurlFollowLocation True)
  setopt h (CurlFailOnError True)
  setopt h (CurlPost True)
  setopt h (CurlPostFields fields')
  setDefaultSSLOpts h url
  setopt h (CurlURL url)
  setopt h (CurlWriteFunction (gatherOutput ref))
  mapM_ (setopt h) opts
  rc <- perform h
  lss <- readIORef ref
  return (rc, concat $ reverse lss)
 where fields' = map (\(x,y) -> x ++ '=':y) fields

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