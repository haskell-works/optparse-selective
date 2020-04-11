{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}

module Options.Applicative.Types
  ( ParserInfo(..)
  , info
  , Option(..)
  , OptName(..)
  , OptReader(..)
  , Parser(..)
  , P(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import GHC.Generics

data ParserInfo a = ParserInfo
  { infoParser      :: Parser a
  , infoFullDesc    :: Bool
  , infoHeader      :: String
  , infoProgDesc    :: String
  , infoFooter      :: String
  , infoFailureCode :: Int }
  deriving (Functor, Generic)

info :: Parser a -> ParserInfo a
info parser = ParserInfo
  { infoParser = parser
  , infoFullDesc = True
  , infoHeader = ""
  , infoProgDesc = ""
  , infoFooter = ""
  , infoFailureCode = 1
  }

data OptName = OptShort !Char
             | OptLong !String
  deriving (Eq, Ord, Generic)

data Option r a = Option
  { main    :: OptReader r
  , def     :: Maybe a
  , show    :: Bool
  , help    :: String
  , metaVar :: String
  , cont    :: r -> Maybe (Parser a)
  }
  deriving (Functor, Generic)

data OptReader a
  = OptReader [OptName] (String -> Maybe a)
  | FlagReader [OptName] !a
  | ArgReader (String -> Maybe a)
  | CmdReader [String] (String -> Maybe (ParserInfo a))
  deriving Functor

data Parser a where
  NilP :: a -> Parser a
  ConsP :: Option r (a -> b)
        -> Parser a
        -> Parser b

instance Functor Parser where
  fmap f (NilP x)      = NilP (f x)
  fmap f (ConsP opt p) = ConsP (fmap (f.) opt) p

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  ConsP opt p1 <*> p2 =
    ConsP (fmap uncurry opt) $ (,) <$> p1 <*> p2

data P a
  = ParseError
  | ParseResult a
  deriving (Functor, Generic)

instance Monad P where
  return = ParseResult
  ParseError >>= _ = ParseError
  ParseResult a >>= f = f a

instance MonadFail P where
  fail _ = ParseError

instance Applicative P where
  pure = return
  (<*>) = ap
