module Main where

import qualified Springer.FenParserTest
import Test.Hspec
import Test.QuickCheck
import Text.ParserCombinators.ReadP (readP_to_S)

main :: IO ()
main = hspec $ do
  Springer.FenParserTest.test

-- describe "isReady" $ do
--   it "parses isReady" $
--     readP_to_S isReady "isready" `shouldBe` [(IsReady, "")]
--   it "" $
--     readP_to_S isReady "notready" `shouldBe` []

-- it "handles multible words" $ do
--   tokenize "debug on" `shouldBe` ["debug", "on"]
--   tokenize "setvalue foo bar" `shouldBe` ["setvalue", "foo", "bar"]
-- it "removes whitespace" $
--   tokenize "debug   on" `shouldBe` ["debug", "on"]
-- it "request" $
--   parseMaybe request "on\n" `shouldBe` Just ["debug", "on"]
-- describe "parse" $ do
--   it "handles simple cases" $ do
--     unify ["?x"] ["?y"] []
--       `shouldBe` [("?x", "?y")]
--     unify ["?x", "+", "1"] ["2", "+", "?y"] []
--       `shouldBe` [("?y", "1"), ("?x", "2")]
--     unify ["?x", "?x", "?x"] ["?y", "?y", "?y"] []
--       `shouldBe` [("?x", "?y")]
--     unify ["?x", "?y", "a"] ["?y", "?x", "?x"] []
--       `shouldBe` [("?y", "a"), ("?x", "?y")]
