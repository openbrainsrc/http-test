module Test.HTTP where

import Network.Curl hiding (curlGetString)
import Network.Browser
import Network.HTTP
import Network.URI    ( parseURI )

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import GHC.Conc
import qualified Text.JSON       as J
import qualified Text.JSON.Types as JT
import Safe (readMay)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Error

-- | Processes all files, possibly in parallel

processAllFiles :: Options -> IO ()
processAllFiles opts = do
  -- Create shared program state allowing safe-thread modification
  let state' = ProgramState 0 (threads opts) [] (length (files opts))
  state <- newTVarIO state'

  -- Run (possibly) parallel tests
  mapM_ (myFork state . processFile state opts) (files opts)

  -- Wait for all children to finish
  state'' <- atomically $ do st <- readTVar state
                             let n = numFiles st
                             when (n > 0) $ retry
                             return st

  -- Results and exit
  printAllTestResults state''
  exitWith $ exitCodes state''

 where myFork st io             = forkFinally io (const $ safelyDecreaseFileNum st)
       safelyDecreaseFileNum st = (atomically $ modifyTVar st decreaseFileNum)
       decreaseFileNum st@(ProgramState { numFiles = nfs }) = st { numFiles = nfs - 1}

processFile :: ProgramStateM -> Options -> FilePath -> IO ()
processFile state opts fp = do
  -- We put processDoc out of the catch so that errors in
  -- processDoc are not treated as missing files
  doc <- catchIOError (fmap Just $ readFile fp)
                      (\e -> handleFileNotExists fp e >> return Nothing)
  maybe (return ()) (processDoc state opts fp) doc

processDoc :: ProgramStateM -> Options -> FilePath -> String -> IO ()
processDoc state opts fp doc = do
  atomically $ do st <- readTVar state 
                  let n = numThreads st
                  when (n >= maxNumThreads st) $ retry
                  writeTVar state (st { numThreads = n + 1 })

  rs <- processDoc' opts fp doc

  atomically $ do st <- readTVar state
                  let n   = numThreads st
                      rss = results st
                  writeTVar state (st { numThreads = n - 1, results = ((fp, rs):rss) })

processDoc' :: Options -> FilePath -> String -> IO ParsingResult
processDoc' opts fp doc = withCurlDo $ do
    c <- initialize
    setopts c curlOpts
  
    -- Open website
    (_,rsp) <- curlGetString c host curlOpts -- ((CurlCookieSession True):curlOpts)
    assertNot "Keter proxies to BayesHive app" 
              ("Welcome to Keter" `isInfixOf` rsp)
              rsp

    assert "Correct blog link" 
              ("href=\"https://bayeshive.com/blog\"" `isInfixOf` rsp)
              ("Incorrect blog link \n")

    -- Login
    (_,rsp) <- curlPostString c (host++"/auth/page/email/login") (curlOpts)
                 [ ("email", username opts)
                 , ("password", pass opts)
                 ]
  
    -- ASSERT: login ok
    assert "Dashboard after login" 
              ("<title>BayesHive Dashboard" `isInfixOf` rsp)
              ("\nLogin failed. "++
               "\nDid you populate the database with users (populate_users.sh)?"++
               "\nDid you redirect bayeshive.com to 127.0.0.1 in /etc/hosts ?\n")
  
    ---- Create doc
  
    (_,rsp) <- curlPostString c (host++"/doc/MyFirstDocument/edit") (curlOpts)
        [("docContents", urlEncode doc)]
  
    -- Clear cache
    _ <- curlPostString c (host++"/doc/MyFirstDocument/clearcache") curlOpts []
    _ <- curlGetString c (host++"/doc/MyFirstDocument/view") curlOpts
  
    -- Reload until processed
    (_,rsp) <- reloadLoop c 45 host curlOpts

    assert "No sprintf errors, NaNs, infinity, " 
           (not $ or $ map (`isInfixOf` rsp) $ words "[sprintf] NaN undefined Infinity") 
           ("Numeric errors: "++rsp)

    let decoded = decode rsp
  
    -- Show result
    --putStrLn decoded
  
    -- ASSERT: estimation ok
    --assert "Estimation successful."
    --       ("pars =\\u003e {"`isInfixOf` rsp)
    --       ("")
    let result = parseExecResult fp $ rsp

    return result

 where host       = proto ++ "://bayeshive.com"
       proto      = if useHttps opts then "https" else "http"
       cookies    = CurlCookieJar "cookies"
       redirects  = CurlFollowLocation True
       curlOpts   = [cookies, redirects]

-- | Attempts to retrieves the document status, with a 1-sec delay between
-- retries, until its estimation is completed
reloadLoop :: Curl -> Int -> URLString -> [CurlOption] -> IO (CurlCode, String)
reloadLoop c retries host opts = do
  (u,rsp) <- curlGetString c (host++"/doc/MyFirstDocument/status") opts
  --putStrLn $ "STATUS: "++ show (u, rsp)
  if rsp /= "Done" && retries > 0 -- ("div class=\\\"reloadmsg\\\"" `isInfixOf` rspBody rsp)
     then do liftIO $ do putStrLn $ "STATUS: "++rsp
                         threadDelay $ 1000*1000*1
             reloadLoop c (retries -1) host opts
     else do curlGetString c (host++"/doc/MyFirstDocument/view") opts

-- | Reports that the file given as arg does not exist
-- (it does not exit because in parallel tests, you may want to execute the rest of the tests)
handleFileNotExists :: FilePath -> IOError -> IO ()
handleFileNotExists fp err = 
  hPutStrLn stderr $ "Cannot access: " ++ fp ++ ": File does not exist or not a file"

{-------------------------------------------------------
                    PRINTING RESULTS
 -------------------------------------------------------}

printAllTestResults :: ProgramState -> IO ()
printAllTestResults st =
  mapM_ (uncurry printTestResults) $ results st

printTestResults :: FilePath -> ParsingResult -> IO ()
printTestResults fp (Left msg) =
  putStrLn $ "FAIL: Simulation results obtained for: " ++ fp ++ ": The error message was: " ++ formatMsg ". " msg
printTestResults fp (Right res) = do
  putStrLn $ "PASS: Simulation results obtained for: " ++ fp
  mapM_ printTestResult res

printTestResult :: (String, Bool) -> IO ()
printTestResult (assertion, result) = putStrLn $ resMsg ++ decode assertion
 where resMsg = if result then "PASS: " else "FAIL: "

formatMsg :: String -> String -> String
formatMsg sep = joinWith sep . filter (not.null) . map trim . lines
 where trim      = reverse . trimLeft . reverse . trimLeft
       trimLeft  []       = []
       trimLeft  (' ':xs) = trimLeft xs
       trimLeft  xs       = xs

       joinWith sep = concat . intersperse sep

{-------------------------------------------------------
                  PROGRAM EXIT CODE
 -------------------------------------------------------}
exitCodes :: ProgramState -> ExitCode
exitCodes = maximum . map (exitCode . snd) . results

exitCode :: ParsingResult -> ExitCode
-- exitCode (Left _)         = ExitFailure 3 -- Error parsing the results
exitCode (Left _) = ExitFailure 2 -- The server refused the file
exitCode (Right xs)
  | all snd xs = ExitSuccess   -- all assertions passed
  | otherwise  = ExitFailure 1 -- some assertions failed

{-------------------------------------------------------
             EXECUTION FLAGS AND CLI PARSING
 -------------------------------------------------------}

-- | Execution conf
data Options = Options
  { username :: String
  , pass     :: String
  , useHttps :: Bool
  , files    :: [String]
  , threads  :: Int
  }
 deriving (Show)

defaultOptions :: Options
defaultOptions = Options "tomn%2Buser%40openbrain.org" "lt5tad" False [] 1

-- Possible execution flags
data Flag = UserName String
          | Password String
          | UseHTTPS
          | Help
          | Parallel Int
  deriving (Eq, Show)

-- | Transforms one CLI flag into a function that transforms the
-- execution options
flagToOptions :: Flag -> Options -> Options
flagToOptions (UserName u) opts = opts { username = urlEncode u }
flagToOptions (Password p) opts = opts { pass     = urlEncode p }
flagToOptions UseHTTPS     opts = opts { useHttps = True }
flagToOptions (Parallel n) opts = opts { threads  = n    }
flagToOptions _            opts = opts

-- | Processes args
parseArgs :: IO Options
parseArgs = do
  argv <- getArgs
  case parse argv of
    ([],   fs, [])    -> return $ defaultOptions { files = fs } -- No flags given
    (opts, fs, [])
      | Help `elem` opts -> help
      | not (null [ x | Parallel x <- opts, x < 0]) -> die [wrongThreadNum]
      | otherwise        -> return $ foldr flagToOptions (defaultOptions{files=fs}) opts
    (_,_,errs)           -> die errs
  where parse argv     = getOpt Permute options argv
        header         = "Usage: TestBayeshive [OPTION] [FILE]"
        wrongThreadNum = "ERROR: The number of max parallel threads must be a natural number (zero means unlimited)\n"
        info           = usageInfo header options
        dump           = hPutStrLn stderr
        die errs       = dump (concat errs ++ info) >> exitWith (ExitFailure 1) 
        help           = dump info                  >> exitWith (ExitFailure 1) 

-- Command-line arguments
options :: [OptDescr Flag]
options = [ Option ['u'] ["username"] (ReqArg UserName "USER")
             "Specifies the user to log in on the website"
          , Option ['p'] ["password"] (ReqArg Password "PASS")
             "Specifies the password to log in on the website"
          , Option ['s'] ["https"] (NoArg UseHTTPS)
             "Uses HTTPS to access the website (default: no)"
          , Option ['h'] ["help"]  (NoArg Help)
             "Shows this help message"
          , Option ['N'] ["parallel"]  (OptArg parseNumThreads "NUM")
             "Runs (up to) NUM tests in parallel (default: 1)"
          ]
 where parseNumThreads = Parallel . fromMaybe (-1) . maybe (Just 1) readMay

{-------------------------------------------------------
                UTILITY FUNCTIONS
 -------------------------------------------------------}

assertNot nm p more = assert nm (not p) more

assert nm p more = 
     liftIO $ if p
                 then putStrLn $ "PASS: "++nm
                 else do putStrLn more
                         fail $ "FAIL: "++nm

{-------------------------------------------------------
        PARALLEL TESTS AND THREAD-SAFE STATE
 -------------------------------------------------------}

type Results = [(String, ParsingResult)]

data ProgramState = ProgramState { numThreads    :: Int
                                 , maxNumThreads :: Int
                                 , results       :: Results
                                 , numFiles      :: Int
                                 }

type ProgramStateM = TVar ProgramState

{-------------------------------------------------------
                  PARSING SERVER RESPONSE
 -------------------------------------------------------}

type ParsingResult = Either
                       String           -- Errors detected by the server
                       [(String, Bool)] -- Assertions

-- | Parses the result of executing a document
parseExecResult :: FilePath -> String -> ParsingResult
parseExecResult fp s =
  case jsonObject of
   J.Error msg            -> Left msg
   J.Ok    (J.JSObject o) -> case JT.get_field o "Right" of
                               (Just (J.JSString x)) -> parseExecResult' fp $ J.fromJSString x
                               _                     -> Left noRightFieldErrMsg
  where -- jsstringToAssertionResults = findAssertions . lines . J.fromJSString
        jsonObject :: J.Result J.JSValue
        jsonObject = J.decode s

        noRightFieldErrMsg = "The response provided by the server does not contain a field Right in the JSON structure"

parseExecResult' :: FilePath -> String -> ParsingResult
parseExecResult' fp s
  | isError s = Left $ eliminateTags $ unlines $ tail $ lines s
  | otherwise = Right $ findAssertions $ lines s

isError :: String -> Bool
isError s = "Error processing" `isInfixOf` s

--findAssertions :: [String] -> [(String, Bool)]
--findAssertions = map assertionResult . filter isAssertion
--
--assertionResult :: String -> (String, Bool)
--assertionResult s = (s, "Pass; True" `isInfixOf` s)

isAssertion :: String -> Bool
isAssertion s = "assert_that" `isInfixOf` s

eliminateTags :: String -> String
eliminateTags []       = []
eliminateTags ('<':xs) = eliminateTags' xs
eliminateTags (x:xs)   = x : eliminateTags xs

eliminateTags' :: String -> String 
eliminateTags' []       = []
eliminateTags' ('>':xs) = eliminateTags xs
eliminateTags' (_:xs)   = eliminateTags' xs


-- Alternative version that finds multi-line assertions:

findAssertions :: [String] -> [(String, Bool)]
findAssertions [] = []
findAssertions ls@(x:xs)
  | isAssertion x && isJust result = (x,res) : findAssertions xs'
  | otherwise                      = findAssertions xs
 where result          = findResult ls
       Just (res, xs') = result

findResult :: [String] -> Maybe (Bool, [String])
findResult [] = Nothing
findResult (x:xs)
  | "Pass; True"  `isInfixOf` x = Just (True, xs)
  | "Fail: False" `isInfixOf` x = Just (False, xs)
  | otherwise                   = findResult xs

   
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