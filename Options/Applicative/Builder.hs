{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Options.Applicative.Builder (
  -- * Readers
  auto,
  str,
  disabled,
  -- * Combinators
  short,
  long,
  help,
  value,
  metavar,
  reader,
  hide,
  multi,
  command,
  idm,
  (&),
  -- * Parsers
  subparser,
  argument,
  arguments,
  flag,
  nullOption,
  strOption,
  option
  ) where

import Control.Applicative
import Control.Category
import Control.Lens                      hiding (argument, (&))
import Data.Generics.Product.Any
import GHC.Generics
import Options.Applicative
import Options.Applicative.Builder.Types (CommandFields (CommandFields), FlagFields (FlagFields), HasName, OptionFields (OptionFields))
import Options.Applicative.Types         hiding (help)
import Prelude                           hiding (id, (.))

import qualified Control.Lens                      as L
import qualified Options.Applicative.Builder.Types as T
import qualified Options.Applicative.Types         as T

data Mod f r a b = Mod (f r -> f r) (Option r a -> Option r b)

optionMod :: (Option r a -> Option r b) -> Mod f r a b
optionMod = Mod id

fieldMod :: (f r -> f r) -> Mod f r a a
fieldMod f = Mod f id

instance Category (Mod f r) where
  id = Mod id id
  Mod f1 g1 . Mod f2 g2 = Mod (f1 . f2) (g1 . g2)

-- readers --

auto :: Read a => String -> Maybe a
auto arg = case reads arg of
  [(r, "")] -> Just r
  _         -> Nothing

str :: String -> Maybe String
str = Just

disabled :: String -> Maybe a
disabled = const Nothing

-- combinators --

short :: HasName f => Char -> Mod f r a a
short = fieldMod . T.name . OptShort

long :: HasName f => String -> Mod f r a a
long = fieldMod . T.name . OptLong

value :: a -> Mod f r a a
value a = optionMod (L.& the @"def" ?~ a)

help :: String -> Mod f r a a
help s = optionMod (L.& the @"help" .~ s)

reader :: (String -> Maybe r) -> Mod OptionFields r a a
reader f = fieldMod (L.& the @"reader" .~ f)

metavar :: String -> Mod f r a a
metavar s = optionMod (L.& the @"metaVar" .~ s)

hide :: Mod f r a a
hide = optionMod (L.& the @"show" .~ False)

multi :: Mod f r a [a]
multi = optionMod f
  where
    f opt = mkOptGroup []
      where
        mkOptGroup xs = opt
          { def = Just xs
          , cont = mkCont xs }
        mkCont xs r = do
          p' <- (opt ^. the @"cont") r
          x <- evalParser p'
          return $ liftOpt (mkOptGroup (x:xs))

command :: String -> ParserInfo r -> Mod CommandFields r a a
command cmd pinfo = fieldMod $ (L.& the @"commands" %~ ((cmd, pinfo):))

-- parsers --

baseOpts :: OptReader a -> Option a a
baseOpts opt = Option
  { T.main = opt
  , T.metaVar = ""
  , T.show = True
  , T.cont = Just . pure
  , T.help = ""
  , T.def = Nothing
  }

subparser :: Mod CommandFields a a b -> Parser b
subparser m = liftOpt . g . baseOpts $ opt
  where
    Mod f g = m . metavar "COMMAND"
    CommandFields cmds = f (CommandFields [])
    opt = CmdReader (map fst cmds) (`lookup` cmds)

argument :: (String -> Maybe a) -> Mod f a a b -> Parser b
argument p (Mod _ g) = liftOpt . g . baseOpts $ ArgReader p

arguments :: (String -> Maybe a) -> Mod f a [a] b -> Parser b
arguments p m = argument p (m . multi)

flag :: a -> Mod FlagFields a a b -> Parser b
flag x (Mod f g) = liftOpt . g . baseOpts $ rdr
  where
    rdr = let fields = f (FlagFields [])
          in FlagReader (fields ^. the @"names") x

nullOption :: Mod OptionFields a a b -> Parser b
nullOption (Mod f g) = liftOpt . g . baseOpts $ rdr
  where
    rdr = let fields = f (OptionFields [] disabled)
          in OptReader (fields ^. the @"names") (fields ^. the @"reader")

strOption :: Mod OptionFields String String a -> Parser a
strOption m = nullOption $ m . reader str

option :: Read a => Mod OptionFields a a b -> Parser b
option m = nullOption $ m . reader auto

idm :: Mod f r a a
idm = id

(&) :: Mod f r a b -> Mod f r b c -> Mod f r a c
(&) = flip (.)
