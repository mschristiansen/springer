module Springer.FenParserTest where

import Springer.Board
import Springer.FenParser
import Springer.Parse
import Test.Hspec

test :: SpecWith ()
test = do
  describe "parse" $ do
    it "can parse the starting position" $
      let fen =
            FenPosition
              { fenBoard = startPosition,
                fenActivePlayer = WhitePlayer,
                fenCastling = [BlackQueenCastling, BlackKingCastling, WhiteQueenCastling, WhiteKingCastling],
                fenEnPassant = NoEnPassant,
                fenHalfMoves = 0,
                fenFullMoves = 1
              }
       in parseWith fenPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" `shouldBe` fen

    it "can parse after the move 1.e4" $
      let fen =
            FenPosition
              { fenBoard = regularMove (FE, R2) (FE, R4) startPosition,
                fenActivePlayer = BlackPlayer,
                fenCastling = [BlackQueenCastling, BlackKingCastling, WhiteQueenCastling, WhiteKingCastling],
                fenEnPassant = EnPassant "e3",
                fenHalfMoves = 0,
                fenFullMoves = 1
              }
       in parseWith fenPosition "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" `shouldBe` fen

    it "can parse after 1...c5" $
      let fen =
            FenPosition
              { fenBoard = regularMove (FC, R7) (FC, R5) $ regularMove (FE, R2) (FE, R4) startPosition,
                fenActivePlayer = WhitePlayer,
                fenCastling = [BlackQueenCastling, BlackKingCastling, WhiteQueenCastling, WhiteKingCastling],
                fenEnPassant = EnPassant "c6",
                fenHalfMoves = 0,
                fenFullMoves = 2
              }
       in parseWith fenPosition "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2" `shouldBe` fen

    it "can parse after 2.Nf3" $
      let fen =
            FenPosition
              { fenBoard = regularMove (FG, R1) (FF, R3) $ regularMove (FC, R7) (FC, R5) $ regularMove (FE, R2) (FE, R4) startPosition,
                fenActivePlayer = BlackPlayer,
                fenCastling = [BlackQueenCastling, BlackKingCastling, WhiteQueenCastling, WhiteKingCastling],
                fenEnPassant = NoEnPassant,
                fenHalfMoves = 1,
                fenFullMoves = 2
              }
       in parseWith fenPosition "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2" `shouldBe` fen
