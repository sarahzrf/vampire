{-# LANGUAGE ImplicitParams, RankNTypes #-}

module Debug.Vampire.Visualize (structFor, toGraph, viewExpr) where

import Debug.Vampire.Data
import Debug.Vampire.Trace
import Control.DeepSeq
import Data.IORef
import Data.Graph.Inductive
import Data.GraphViz hiding (parse)

-- imports for copypasted func
import Data.DList (singleton, fromList, toList)
import Control.Arrow
import Control.Monad.RWS

instance Labellable () where
  toLabelValue = const (toLabelValue "")

structFor :: (Show a, NFData a) => ((?vCtx::IORef ExprStruct') => () -> a) -> IO ExprStruct
structFor d = do
  let struct = (let ?vCtx = vNewExprStruct "toplevel" in d () `deepseq` ?vCtx)
  struct' <- readIORef struct >>= resolve
  return $ case children' struct' of
    full:_ -> full
    []     -> ExprStruct "" Nothing []

labelFor :: ExprStruct -> String
labelFor (ExprStruct expr (Just val) _) = expr ++ " = " ++ val
labelFor (ExprStruct expr Nothing _) = expr ++ " = Unevaluated"

-- copypasted from SO, credit: http://stackoverflow.com/a/14621912
toGraph :: ExprStruct -> Gr String ()
toGraph t = uncurry mkGraph . (toList *** toList) . snd $ evalRWS (go t) () [1..]
  where go e@(ExprStruct _ _ ns) = do
          i <- state $ head &&& tail
          es <- forM ns $ go >=> \j -> return (i, j, ())
          tell (singleton (i, labelFor e), fromList es)
          return i

viewExpr :: (Show a, NFData a) => ((?vCtx::IORef ExprStruct') => () -> a) -> IO ()
viewExpr = structFor >=> preview . toGraph

