import Test.HTTP
import Data.List (isInfixOf)

main = httpTest $ do
  session "BayesHive landing page" "https://bayeshive.com" $ do
    landing <- get "/"
    assert "Correct blog link" $ 
           "href=\"https://bayeshive.com/blog\"" `isInfixOf` landing
    loginResult <- postForm "/auth/page/email/login" 
              [("email", "foo"), ("password", "bar")]
    debug loginResult
