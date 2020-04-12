module CSDC.Error
  ( Error (..)
  , IsError (..)
  , HasError
  ) where

import Control.Monad.Except (MonadError)

data Error = NetworkError String
  deriving (Show, Eq)

class IsError e where
  toError :: e -> Error

type HasError e m = (MonadError Error m, IsError e)
