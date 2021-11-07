-- The UCI protocol is designed for communication between a GUI and an
-- Engine. The GUI and the Engine can send and receive certain
-- commands, which here will be named GuiCommands and EngineCommands.

-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-ParserCombinators-ReadP.html
-- https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html
-- https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol/

module Springer.UciParser (guiCommandParser, GuiCommand (..)) where

import Control.Applicative ((<|>))
import Data.Char (isLetter)
import Data.Functor (($>))
import Springer.FenParser (FenPosition, fenPosition)
import Springer.Parse

data GuiCommand
  = Uci
  | Debug (Maybe Bool)
  | SetOption String (Maybe String)
  | Register
  | IsReady
  | UciNewGame
  | PositionStart
  | Position FenPosition
  | Go [GoCommand]
  | Stop
  | PonderHit
  | Quit
  deriving (Show, Eq)

uci :: ReadP GuiCommand
uci = string "uci" $> Uci

-- Example: "debug" || "debug off"
debug :: ReadP GuiCommand
debug = do
  string "debug"
  skipSpaces
  opt <- option Nothing (Just <$> true <|> Just <$> false)
  return $ Debug opt

true :: ReadP Bool
true = string "on" $> True

false :: ReadP Bool
false = string "off" $> False

-- Example: "setoption name UCI_AnalyseMode value true"
setOption :: ReadP GuiCommand
setOption = do
  string "setoption"
  skipSpaces
  string "name"
  skipSpaces
  name <- kv
  value <- option Nothing $ do
    skipSpaces
    string "value"
    skipSpaces
    Just <$> kv
  pure $ SetOption name value

isReady :: ReadP GuiCommand
isReady = string "isready" $> IsReady

uciNewGame :: ReadP GuiCommand
uciNewGame = string "ucinewgame" $> UciNewGame

-- Example: "position startpos moves e2e4 e7e5"
positionStart :: ReadP GuiCommand
positionStart = do
  string "position"
  skipSpaces
  string "startpos"
  pure PositionStart

positionFen :: ReadP GuiCommand
positionFen = do
  string "position"
  skipSpaces
  string "fen"
  skipSpaces
  Position <$> fenPosition

-- Examples: e2e4, e7e5, e1g1 (white short castling), e7e8q (for
-- promotion)
move :: ReadP String
move = do
  let isFile f = f `elem` "abcdefgh"
      isRank r = r `elem` "12345678"
  f <- satisfy isFile
  r <- satisfy isRank
  g <- satisfy isFile
  s <- satisfy isRank
  return [f, r, g, s]

-- Example: "go infinite"
go :: ReadP GuiCommand
go = do
  string "go"
  skipSpaces
  pure $ Go [GoInfinite]

data GoCommand
  = GoSearchMoves String
  | GoPonder
  | GoWhiteTime Int
  | GoBlackTime Int
  | GoWhiteIncrement Int
  | GoBlackIncrement Int
  | GoMovesToGo Int
  | GoDepth Int
  | GoNodes Int
  | GoMate Int
  | GoMoveTime Int
  | GoInfinite
  deriving (Show, Eq)

stop :: ReadP GuiCommand
stop = string "stop" $> Stop

request :: ReadP [GuiCommand]
-- request = sepBy oneRequest skipSpaces
request = many (skipSpaces >> oneRequest)

oneRequest :: ReadP GuiCommand
oneRequest = choice [uci, setOption, isReady, debug, uciNewGame, positionStart, positionFen, go, stop]

guiCommandParser :: ReadP GuiCommand
guiCommandParser = oneRequest

kv :: ReadP String
kv = munch1 isLetter

-- Should only be sent after the engine has received a position
data GuiPositionCommand
  = SearchMoves
  | Ponder
  | WhiteTime Int
  | BlackTime Int
  | WhiteIncrement Int
  | BlackIncrement Int

data EngineCommand
  = Id
  | Option
  | UciOK

newtype UciMove = UciMove String deriving (Show)

-- | Examples:  e2e4, e7e5, e1g1 (white short castling), e7e8q (for promotion)
uciMove :: ReadP UciMove
uciMove = do
  a <- koor
  b <- koor
  pure $ UciMove (a <> b)

koor :: ReadP String
koor = do
  let isFile f = f `elem` "abcdefgh"
      isRank r = r `elem` "12345678"
  f <- satisfy isFile
  r <- satisfy isRank
  return [f, r]
