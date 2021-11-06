-- https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
-- See also section 16.1 in the PGN Standard(https://ia802908.us.archive.org/26/items/pgn-standard-1994-03-12/PGN_standard_1994-03-12.txt)
module Springer.FenParser (fenPosition, FenPosition (..), ActivePlayer (..), Castling (..), EnPassant (..)) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (digitToInt, isDigit)
import Data.Functor (($>))
import Springer.Board
import Springer.Parse

data FenPosition = FenPosition
  { fenBoard :: Board,
    fenActivePlayer :: ActivePlayer,
    fenCastling :: [Castling],
    fenEnPassant :: EnPassant,
    fenHalfMoves :: Int,
    fenFullMoves :: Int
  }
  deriving (Show, Eq)

fenPosition :: ReadP FenPosition
fenPosition = do
  p <- placement
  space
  a <- activePlayer
  space
  c <- castling
  space
  e <- enPassant
  space
  hm <- moveCount
  space
  fm <- moveCount
  pure $ FenPosition p a c e hm fm

placement :: ReadP Board
placement = Board . go [] <$> munch1 isPlacement
  where
    isPlacement c = c `elem` "prnbqkPRNBQK12345678/"
    go acc [] = acc
    go acc (p : ps) = go (acc <> charPieces p) ps
    charPieces :: Char -> [Square]
    charPieces ch =
      case ch of
        'p' -> [Black Pawn]
        'r' -> [Black Rook]
        'n' -> [Black Knight]
        'b' -> [Black Bishop]
        'q' -> [Black Queen]
        'k' -> [Black King]
        'P' -> [White Pawn]
        'R' -> [White Rook]
        'N' -> [White Knight]
        'B' -> [White Bishop]
        'Q' -> [White Queen]
        'K' -> [White King]
        '/' -> []
        n -> replicate (digitToInt n) Empty

data ActivePlayer = WhitePlayer | BlackPlayer deriving (Show, Eq)

activePlayer :: ReadP ActivePlayer
activePlayer =
  char 'w' $> WhitePlayer <|> char 'b' $> BlackPlayer

data Castling
  = NoCastling
  | WhiteQueenCastling
  | WhiteKingCastling
  | BlackQueenCastling
  | BlackKingCastling
  deriving (Show, Eq)

castling :: ReadP [Castling]
castling = go [] <$> munch1 isCastle
  where
    isCastle c = c `elem` "-QKqk"
    go acc [] = acc
    go acc (c : cs) = go (fromChar c : acc) cs
    fromChar ch =
      case ch of
        '-' -> NoCastling
        'Q' -> WhiteQueenCastling
        'K' -> WhiteKingCastling
        'q' -> BlackQueenCastling
        'k' -> BlackKingCastling

data EnPassant = NoEnPassant | EnPassant String deriving (Show, Eq)

enPassant :: ReadP EnPassant
enPassant = (char '-' $> NoEnPassant) <|> EnPassant <$> move

move :: ReadP String
move = do
  let isFile f = f `elem` "abcdefgh"
      isRank r = r `elem` "12345678"
  f <- satisfy isFile
  r <- satisfy isRank
  return [f, r]

moveCount :: ReadP Int
moveCount = read <$> munch1 isDigit

space :: ReadP ()
space = void $ char ' '
