module Main where

import Debug.Vampire
import Options.Applicative
import Control.Applicative
import Control.Monad
import System.Directory
import System.IO
import System.Process
import System.Exit
import Paths_vampire

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

doREPL :: String -> IO ()
doREPL mod = do
  script <- getDataFileName "vampire-repl"
  () <$ rawSystem "ghci" [mod, "-ghci-script", script]

doRewriteREPL :: String -> IO ()
doRewriteREPL src = do
  (dest, h) <- getTemporaryDirectory >>= flip openTempFile "Drained.hs"
  hClose h
  doRewrite src dest
  doREPL dest

-- parsers!

rewrite =
  liftA2 doRewrite
    (argument str $ metavar "SOURCE")
    (argument str $ metavar "DEST" <> value "-" <> showDefault)
rewriteDesc = "Rewrite a Haskell source file to add Vampire tracing."
repl =
  fmap doREPL
    (argument str $ metavar "MODULE")
replDesc = "Open a GHCi with a few custom commands onto a rewritten module."
reREPL =
  fmap doRewriteREPL
    (argument str $ metavar "SOURCE")
reREPLDesc =
  "Rewrite a Haskell source file to add Vampire tracing, then " ++
  "open a GHCi with a few custom commands onto the rewritten module."
 
vampire = subparser $
  (command "rewrite" $ info rewrite $ progDesc rewriteDesc) <>
  (command "repl"    $ info repl    $ progDesc replDesc)    <>
  (command "rerepl"  $ info reREPL  $ progDesc reREPLDesc)

main = join $ execParser $
  info (helper <*> vampire)
  (fullDesc <> header "vampire - eeevil expression tree analysis")

