import Grid
import Planning

import System.IO
import System.Environment
import Data.List (intercalate)
import Control.Monad

main = do
  args <- getArgs
  case args of
    [] -> mapM_ (hPutStrLn stderr) [
      "Usage: sokosolve <filename>",
      "File format: ascii; newlines: Unix style",
      " # : Wall",
      " - or space : Clear",
      " @ : Player",
      " $ : Box",
      " . : Target",
      " * : Box on target",
      " + : Player on target,
      "Output: UTF-8 to stdout"
     ]
    (fn:_) -> do
      g <- fmap readGrid $ readFile fn
      case solve g of
        Nothing -> putStrLn "No solution"
        Just s -> putStrLn $ intercalate "," $ map show s

