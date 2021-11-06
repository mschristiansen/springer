module Springer (main) where

import Data.List
import Springer.Parse (parseWith)
import Springer.UciParser (GuiCommand (..), guiCommandParser)
import System.IO

main :: IO ()
main = do
  let logFile = "/tmp/springer.log"
  hSetBuffering stdout NoBuffering
  let go = do
        s <- getLine
        appendFile logFile ("< " <> s <> "\n")
        let resp = handleCommand s
        mapM_ (\s -> appendFile logFile ("> " <> s <> "\n")) resp
        mapM_ putStrLn resp
        go
  go

handleCommand :: String -> [String]
handleCommand s = respond $ parseWith guiCommandParser s

respond :: GuiCommand -> [String]
respond command =
  case command of
    Uci -> ["id name Springer", "id author Mikkel", "uciok"]
    Debug Nothing -> ["debug switch"]
    Debug (Just True) -> ["debug on"]
    Debug (Just False) -> ["debug off"]
    IsReady -> ["readyok"]
    UciNewGame -> []
    PositionStart -> []
    Go _ -> []
    Stop -> []
    PonderHit -> []
    Quit -> []
