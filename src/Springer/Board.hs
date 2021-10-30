module Springer.Board where

import Data.Char (toUpper)

newtype Board = Board [Square] deriving (Show, Eq)

data Square
  = Empty
  | White Piece
  | Black Piece
  deriving (Eq)

data Piece
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Show, Eq)

startPosition :: Board
startPosition =
  Board $
    concat
      [ map Black startPieces,
        replicate 8 (Black Pawn),
        replicate 8 Empty,
        replicate 8 Empty,
        replicate 8 Empty,
        replicate 8 Empty,
        replicate 8 (White Pawn),
        map White startPieces
      ]

startPieces :: [Piece]
startPieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- | Add newlines and display on standard output
displayBoard :: Board -> IO ()
displayBoard (Board squares) = putStr $ rows "" squares
  where
    rows :: String -> [Square] -> String
    rows acc [] = acc
    rows acc xs = rows (acc ++ concatMap show (take 8 xs) ++ "\n") (drop 8 xs)

instance Show Square where
  show Empty = "."
  show (White piece) = [toUpper $ pieceChar piece]
  show (Black piece) = [pieceChar piece]

pieceChar :: Piece -> Char
pieceChar Pawn = 'p'
pieceChar Knight = 'n'
pieceChar Bishop = 'b'
pieceChar Rook = 'r'
pieceChar Queen = 'q'
pieceChar King = 'k'
