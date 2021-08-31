module Board where

import Data.Char (toUpper)

newtype Board = Board [Square]

data Square
  = Empty
  | White Piece
  | Black Piece

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
      [ (map Black startPieces),
        (map Black $ replicate 8 Pawn),
        (replicate 8 Empty),
        (replicate 8 Empty),
        (replicate 8 Empty),
        (replicate 8 Empty),
        (map White $ replicate 8 Pawn),
        (map White $ startPieces)
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
