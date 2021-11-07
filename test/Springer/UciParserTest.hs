module Springer.UciParserTest where

import Springer.Board
import Springer.Parse
import Springer.UciParser
import Test.Hspec

test :: SpecWith ()
test = do
  describe "uci" $
    it "can parse 'uci'" $ parseWith uci "uci" `shouldBe` Uci

  describe "debug" $ do
    it "handles 'debug'" $ parseWith debug "debug" `shouldBe` Debug Nothing
    it "handles 'debug on'" $ parseWith debug "debug on" `shouldBe` Debug (Just True)
    it "handles 'debug off'" $ parseWith debug "debug off" `shouldBe` Debug (Just False)

  describe "setOption" $ do
    it "handles only name" $ parseWith setOption "setoption name springer" `shouldBe` SetOption "springer" Nothing
    it "handles name and value" $ parseWith setOption "setoption name UCI_AnalyseMode value true" `shouldBe` SetOption "UCI_AnalyseMode" (Just "true")
    it "handles setoption name Style value Risky\n" $ parseWith setOption "setoption name Style value Risky\n" `shouldBe` SetOption "Style" (Just "Risky")

  describe "register" $ do
    it "register later" $ parseWith register "register later" `shouldBe` Register
  -- it "register name Stefan MK code 4359874324" $ parseWith register "register name Stefan MK code 4359874324" `shouldBe` Register

  describe "isready" $
    it "can parse 'isready'" $ parseWith isReady "isready" `shouldBe` IsReady

  describe "ucinewgame" $
    it "can parse 'ucinewgame'" $ parseWith uciNewGame "ucinewgame" `shouldBe` UciNewGame

  describe "position" $ do
    it "start position" $ parseWith position "position startpos" `shouldBe` PositionStart []
    it "start position moves e2e4" $ parseWith position "position startpos moves e2e4" `shouldBe` PositionStart ["e2e4"]
    it "start position moves e2e4 d7d6" $ parseWith position "position startpos moves e2e4 d7d6" `shouldBe` PositionStart ["e2e4", "d7d6"]

  describe "go" $ do
    it "go infinite" $ parseWith go "go infinite" `shouldBe` Go [GoInfinite]
