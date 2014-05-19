import Test.HTTP
import Data.List (isInfixOf)

main = defaultMain $ httpTestCase "BayesHive landing page" "https://bayeshive.com" $ do
    landing <- get "/"
    assert "Correct blog link" $ 
           "href=\"https://bayeshive.com/blog\"" `isInfixOf` landing
    loginResult <- postForm "/auth/page/email/login" 
              [("email", "footest@bar.com"), ("password", "secret")] 
    doclist <- getJSON "/doclist"
    debug $ show (doclist:: [String])
    return ()
