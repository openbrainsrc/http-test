module Main where

import Test.HTTP
import Data.List

main = httpTest $ do
  session "BayesHive landing page" "https://bayeshive.com" $ do
    landing <- get "/"
    assert "Correct blog link" $ 
           "href=\"https://bayeshive.com/blog\"" `isInfixOf` landing