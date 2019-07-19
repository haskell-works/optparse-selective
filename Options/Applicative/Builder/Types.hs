{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Options.Applicative.Builder.Types
  ( OptionFields(..)
  , FlagFields(..)
  , CommandFields(..)
  , HasName(..)
  ) where

import Control.Applicative
import Control.Category
import Control.Lens
import Data.Generics.Product.Any
import GHC.Generics
import Options.Applicative
import Options.Applicative.Types
import Prelude                   hiding (id, (.))

import qualified Options.Applicative.Types as T

data OptionFields a = OptionFields
  { names  :: [OptName]
  , reader :: String -> Maybe a
  } deriving Generic

newtype FlagFields a = FlagFields
  { names :: [OptName]
  } deriving Generic

newtype CommandFields a = CommandFields
  { commands :: [(String, ParserInfo a)]
  } deriving Generic

class HasName f where
  name :: OptName -> f a -> f a

instance HasName OptionFields where
  name n = the @"names" %~ (n:)

instance HasName FlagFields where
  name n = the @"names" %~ (n:)
