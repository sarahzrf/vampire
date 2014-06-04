module Main where

import Debug.Vampire
import System.Environment
import Data.Functor

main = do
  args <- getArgs
  case args of
    fn:_ -> do
      newCode <- rewriteFile <$> readFile fn
      case newCode of
        Just code -> putStrLn code
        Nothing -> putStrLn "Invalid syntax in file!"
    [] -> putStrLn "Need a filename!"

