{-# LANGUAGE OverloadedStrings #-}

module Springer.Board where

import qualified Data.Vector as V

newtype Board = Board (V.Vector Square) deriving (Show, Eq)

data Square
  = Empty
  | White Piece
  | Black Piece
  deriving (Show, Eq)

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
    V.fromList $
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
displayBoard (Board squares) = putStr $ rows "" $ V.toList squares
  where
    rows :: String -> [Square] -> String
    rows acc [] = acc
    rows acc xs = rows (acc ++ map squareChar (take 8 xs) ++ "\n") (drop 8 xs)

squareChar :: Square -> Char
squareChar square =
  case square of
    Empty -> '.'
    White Pawn -> 'P'
    Black Pawn -> 'p'
    White Knight -> 'N'
    Black Knight -> 'n'
    White Bishop -> 'B'
    Black Bishop -> 'b'
    White Rook -> 'R'
    Black Rook -> 'r'
    White Queen -> 'Q'
    Black Queen -> 'q'
    White King -> 'K'
    Black King -> 'k'

charSquare :: Char -> Square
charSquare char =
  case char of
    '.' -> Empty
    'P' -> White Pawn
    'p' -> Black Pawn
    'N' -> White Knight
    'n' -> Black Knight
    'B' -> White Bishop
    'b' -> Black Bishop
    'R' -> White Rook
    'r' -> Black Rook
    'Q' -> White Queen
    'q' -> Black Queen
    'K' -> White King
    'k' -> Black King
    _ -> error "charSquare: undefined char"

-- | Makes a move using the index (0-63) of the board
regularIndexMove :: (Int, Int) -> Board -> Board
regularIndexMove (from, to) (Board board) =
  let square = board V.! from in Board $ board V.// [(to, square), (from, Empty)]

-- | Makes a move using long notation (e2e4)
regularMove :: (File, Rank) -> (File, Rank) -> Board -> Board
regularMove from to = regularIndexMove (indexSAN from, indexSAN to)

promoteIndexMove :: (Int, Int) -> Piece -> Board -> Board
promoteIndexMove (from, to) piece (Board board) =
  let square = board V.! from
      swap Empty = Empty
      swap (Black _) = Black piece
      swap (White _) = White piece
   in Board $ board V.// [(to, swap square), (from, Empty)]

promoteMove :: (File, Rank) -> (File, Rank) -> Piece -> Board -> Board
promoteMove from to = promoteIndexMove (indexSAN from, indexSAN to)

castleQueenSideWhite :: Board -> Board
castleQueenSideWhite = king . rook
  where
    king = regularMove (FE, R1) (FC, R1)
    rook = regularMove (FA, R1) (FD, R1)

castleKingSideWhite :: Board -> Board
castleKingSideWhite = king . rook
  where
    king = regularMove (FE, R1) (FG, R1)
    rook = regularMove (FH, R1) (FF, R1)

castleQueenSideBlack :: Board -> Board
castleQueenSideBlack = king . rook
  where
    king = regularMove (FE, R8) (FC, R8)
    rook = regularMove (FA, R8) (FD, R8)

castleKingSideBlack :: Board -> Board
castleKingSideBlack = king . rook
  where
    king = regularMove (FE, R8) (FG, R8)
    rook = regularMove (FH, R8) (FF, R8)

-- | Calculate index in vector from short notation
-- | Examples: E4 = 36 and A1 = 56
indexSAN :: (File, Rank) -> Int
indexSAN (f, r) = 8 * (7 - fromEnum r) + fromEnum f

data File = FA | FB | FC | FD | FE | FF | FG | FH deriving (Show, Eq, Ord, Enum)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Show, Eq, Ord, Enum)
