module Springer (main) where

import Control.Monad (unless)
import Springer.Parse (parseWith)
import Springer.UciParser (GuiCommand (..), guiCommandParser)
import System.IO

main :: IO ()
main = do
  let logFile = "/tmp/springer.log"
  hSetBuffering stdout NoBuffering
  appendFile logFile "Springer: starting...\n"
  commandLoop logFile
  appendFile logFile "Springer: goodbye...\n"

commandLoop :: FilePath -> IO ()
commandLoop logFile = do
  s <- getLine
  appendFile logFile ("gui    : " <> s <> "\n")
  let cmd = parseWith guiCommandParser s
  reply logFile $
    case cmd of
      Uci -> ["id name Springer", "id author Mikkel", "uciok"]
      Debug Nothing -> ["debug switch"]
      Debug (Just True) -> ["debug on"]
      Debug (Just False) -> ["debug off"]
      SetOption _ _ -> ["setoption"]
      Register -> []
      IsReady -> ["readyok"]
      UciNewGame -> []
      PositionStart _ -> []
      Position _fen -> []
      Go _ -> []
      Stop -> []
      PonderHit -> []
      Quit -> ["info goodbye"]
  unless (cmd == Quit) $ commandLoop logFile

reply :: FilePath -> [String] -> IO ()
reply logFile cmds = do
  mapM_ (\l -> appendFile logFile ("engine : " <> l <> "\n")) cmds
  mapM_ putStrLn cmds
