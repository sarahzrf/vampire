module Main where

import Debug.Vampire
import Options.Applicative
import Control.Applicative
import Control.Monad
import System.Exit

readFrom "-"  = getContents
readFrom file = readFile file
writeTo  "-"  = putStrLn
writeTo  file = writeFile file

doRewrite :: String -> String -> IO ()
doRewrite src dest = do
  code <- readFrom src
  case rewriteFile code of
    Just newCode -> writeTo dest newCode
    Nothing      -> putStrLn "could not parse input" >> exitFailure

-- parsers!

rewrite =
  liftA2 doRewrite
    (argument str $ metavar "SOURCE")
    (argument str $ metavar "DEST" <> value "-")
rewriteDesc = "Rewrite a Haskell source file to add Vampire tracing."
 
vampire = subparser $
  (command "rewrite" $ info rewrite $ progDesc rewriteDesc)

main = join $ execParser $
  info (helper <*> vampire)
  (fullDesc <> header "vampire - eeevil expression tree analysis")

