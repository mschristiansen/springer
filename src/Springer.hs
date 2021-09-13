module Springer (main) where

import Data.List
import System.IO
import Springer.UciParser (parse, GuiCommand(..))

main :: IO ()
main = do
  let logFile = "/tmp/springer.log"
  hSetBuffering stdout NoBuffering
  let go = do
        s <- hGetLine stdin
        appendFile logFile ("< " <> s)
        let resp = handleCommand s
        case resp of
          Left err -> appendFile logFile ("e " <> err)
          Right resp' -> do
            mapM_ (appendFile logFile) resp'
            mapM_ putStrLn resp'
        go
  go

handleCommand :: String -> Either String [String]
handleCommand s = do
  case parse s of
    Left err -> Left err
    Right command -> Right $ respond command


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
    Go -> []
    Stop -> []
    PonderHit -> []
    Quit -> []
