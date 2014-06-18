module Debug.Vampire.Data (ExprStruct(..), ExprStruct'(..), resolve) where

import Data.IORef
import Data.Functor

data ExprStruct =
  ExprStruct {expr :: String,
              value :: Maybe String,
              children :: [ExprStruct]} deriving Show
data ExprStruct' =
  ExprStruct' {expr' :: String,
              value' :: Maybe String,
              children' :: [IORef ExprStruct']}

resolve :: ExprStruct' -> IO ExprStruct
resolve (ExprStruct' expr value children) =
  ExprStruct expr value <$> (mapM readIORef children >>= mapM resolve)

