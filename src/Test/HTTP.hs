module Test.HTTP (httpTestCase, get, getJSON, withJSON, post, postJSON, postForm, assert, assertEq, assertParse, debug, Session, Url, Tasty.defaultMain, tic, toc) where

import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Aeson as Ae
import Data.Aeson.Types (Parser, parseEither)
import System.Environment
import System.Exit
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as BL 

import qualified Test.Tasty.HUnit as HUnit
import Test.Tasty as Tasty
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as WreqT
import Control.Lens
import Data.Aeson.Lens
import Data.Time

import qualified Network.HTTP.Client as HT 

import Data.ByteString.Lazy.Char8 (unpack, pack)

type Session = S.StateT HttpTest IO

data HttpTest = HttpTest { baseUrl :: String,
                           cookieJar :: HT.CookieJar,
                           timer :: UTCTime }

type Url = String


httpTestCase :: String -- ^ Session name (used for logging failures)
             -> Url -- ^ Base URL
             -> Session () -- ^ the actions and assertions that define the session
             -> TestTree
httpTestCase sessionName sessBaseURL m =  HUnit.testCase sessionName $ do
   tm <- getCurrentTime
   S.evalStateT m $ HttpTest sessBaseURL (HT.createCookieJar []) tm


withHT :: (HttpTest -> IO (HT.CookieJar, a)) -> Session a
withHT m = do 
  ht <- S.get 
  (cj, x) <- liftIO $  m ht
  S.modify $ \ht -> ht { cookieJar = cj }
  return x

-- | GET a web page as a String
get :: Url -- ^ URL
    -> Session String
get url = do
  (code, res) <- getRaw url
  when (code /= 200) $ 
     assertFailure $ "GET "++url++": "++show code -- ++"\nResponse:\n"++res
  return res



getRaw :: Url -> Session (Int, String) 
getRaw url = withHT $ \(HttpTest base cj _) -> do
   r <- Wreq.getWith (Wreq.defaults & Wreq.cookies .~ cj) (base ++ url)
   return (r ^. Wreq.responseCookieJar, 
           (r ^. Wreq.responseStatus . Wreq.statusCode, 
            unpack $ r ^. Wreq.responseBody))


-- | GET a JSON value
getJSON :: Ae.FromJSON a => 
           Url  -- ^ URL
           -> Session a
getJSON url = do
  str <- get url
  case Ae.eitherDecode' $ BL.fromStrict $ encodeUtf8 $ T.pack str of
    Right x -> return x
    Left err -> throwError $  strMsg $ "GET "++url ++ " JSON decoding failure: "++ err


-- | perform an action with a JSON value from a GET
withJSON :: Ae.FromJSON a => 
           String  -- ^ URL
           -> (a -> Session ()) -- ^ action to perform on successfully decoded value
           -> Session ()
withJSON url mu = do
  str <- get url
  case Ae.eitherDecode' $ BL.fromStrict $ encodeUtf8 $ T.pack str of
    Right x -> mu x
    Left err -> do assertFailure $ "GET "++url++"JSON decoding failure: "++ err
                   return ()


-- | Post a string body
postRaw :: WreqT.Postable a => Url -> a -> Session String
postRaw url body = withHT $ \(HttpTest base cj _) -> do
  r <- Wreq.postWith (Wreq.defaults & Wreq.cookies .~ cj) (base ++ url) body

  let code = r ^. Wreq.responseStatus . Wreq.statusCode

  when (code /= 200) $ 
     liftIO $ HUnit.assertFailure $ "POST "++url++": "++show code++"\nResponse:\n"++show r
  return $ (r ^. Wreq.responseCookieJar, unpack $ r ^. Wreq.responseBody)




-- | Post a form
postForm :: Url -- ^ URL
         -> [(String,String)]  -- ^ form fields
         -> Session String
postForm url fields = postRaw url $  map (\(x,y) -> (BL.toStrict $ pack x, BL.toStrict $ pack y)) fields


-- | Post a string body
post :: Url -> String -> Session String
post u s = postRaw u $ pack s


-- | Post a JSON value
postJSON :: (Ae.ToJSON a, Ae.FromJSON b) => Url -> a -> Session b
postJSON url x = do str <- post url $ T.unpack $ decodeUtf8 $ BL.toStrict $ Ae.encode x
                    case Ae.eitherDecode' $ BL.fromStrict $ encodeUtf8 $ T.pack str of
                      Right x -> return x
                      Left err -> throwError $ strMsg $  "POST "++url ++ " JSON decoding failure: "++ err


-- | make an assertion
assert :: String -- ^ assertion name (used for reporting failures
       -> Bool -- ^ Boolean of which we are asserting truth
       -> Session ()
assert assName True = liftIO $ putStrLn $ "Pass: "++assName
assert assName False = liftIO $ HUnit.assertFailure assName

-- | assert an filure
assertFailure :: String -> Session ()
assertFailure nm = liftIO $ HUnit.assertFailure nm


-- | assert equality, for better output messages
assertEq :: (Show a, Eq a) => String -- ^ assertion name (used for reporting failures
       -> a  -- ^ a value
       -> a  -- ^ what it is meant to be equal to
       -> Session ()
assertEq assName x y | x == y    = liftIO $ putStrLn $ "Pass: "++assName
                     | otherwise =  liftIO $ HUnit.assertFailure $ assName++show x ++" /= "++show y

-- | make an assertion in the Parser monad, for use with JSON value
assertParse :: String      -- ^ assertion name (used for reporting failures
            -> Parser Bool -- ^ Boolean of which we are asserting truth
            -> Session ()
assertParse assName pb  =
  case parseEither (const pb) () of
    Left err -> assertFailure $ assName++" parse failure: "++err
    Right True -> liftIO $ putStrLn $ "Pass: "++assName
    Right False -> assertFailure $ assName++" JSON assertion false"


-- | Output a string to stdout if @--verbose@ is in command line arguments
debug :: String -> Session ()
debug s = liftIO $ putStrLn s

{-  args <- liftIO $ getArgs
  if "--verbose" `elem` args
     then 
     else return () -}


-- | Re-start the timer

tic :: Session ()
tic = do
  now <- liftIO $ getCurrentTime
  S.modify $ \s -> s { timer = now }


-- | Print the number of seconds elapsed, with a prefix

toc :: String -> Session ()
toc s = do
  last <- fmap timer $ S.get
  now <- liftIO $ getCurrentTime
  let df = diffUTCTime now last
  liftIO $ putStrLn $ s ++ " "++show df
