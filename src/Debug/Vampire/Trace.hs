module Debug.Vampire.Trace (vNewExprStruct, vLog) where

import Debug.Vampire.Data
import Data.IORef
import System.IO.Unsafe

instance Show (a -> b) where
  show _ = "[function]"

vNewExprStruct :: String -> IORef ExprStruct'
vNewExprStruct expr =
  unsafePerformIO $ newIORef ExprStruct' {expr' = expr, value' = Nothing, children' = []}

{-# NOINLINE vNewExprStruct #-}

vLog :: Show a => IORef ExprStruct' -> a -> IORef ExprStruct' -> a
vLog parent result current = unsafePerformIO $ do
  modifyIORef' current $ \s -> s {value' = Just (show result)}
  modifyIORef' parent  $ \s@ExprStruct' {children' = c} -> s {children' = current:c}
  return result

{-# NOINLINE vLog #-}

