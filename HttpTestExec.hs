import System.Environment
import qualified Network.Wreq 
import qualified Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Control.Monad
import Network.URI
main = do
   args <- getArgs
   mapM_ dispatch $ filter notOption args

dispatch file = case parseURI file of 
                  Nothing -> runFile file 
                  Just _ -> runHttp file




isOption ('-':_) = True
isOption _ = False

notOption = not . isOption
