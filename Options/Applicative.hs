{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PatternGuards    #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}

module Options.Applicative
  ( Parser

  , ParserInfo(..)
  , info

  , evalParser
  , runParser
  , liftOpt
  , mapParser
  , optionNames
  ) where

import Control.Applicative
import Control.Lens              hiding (uncons)
import Data.Generics.Product.Any
import Data.Maybe
import Data.Monoid
import Options.Applicative.Types

optionNames :: OptReader a -> [OptName]
optionNames (OptReader names _)  = names
optionNames (FlagReader names _) = names
optionNames _                    = []

liftOpt :: Option r a -> Parser a
liftOpt opt = ConsP (fmap const opt) (pure ())

uncons :: [a] -> Maybe (a, [a])
uncons []       = Nothing
uncons (x : xs) = Just (x, xs)

data MatchResult
  = NoMatch
  | Match (Maybe String)

instance Semigroup MatchResult where
  (<>) m@(Match _) _ = m
  (<>) _ m           = m

instance Monoid MatchResult where
  mempty = NoMatch
  mappend m@(Match _) _ = m
  mappend _ m           = m

type Matcher a = [String] -> P (a, [String])

optMatches :: OptReader a -> String -> Maybe (Matcher a)
optMatches rdr arg = case rdr of
  OptReader names f
    | Just (arg1, val) <- parsed
    , arg1 `elem` names
    -> Just $ \args -> do
         (arg', args') <- tryP . uncons $ maybeToList val ++ args
         r <- tryP $ f arg'
         return (r, args')
    | otherwise -> Nothing
  FlagReader names x
    | Just (arg1, Nothing) <- parsed
    , arg1 `elem` names
    -> Just $ \args -> return (x, args)
  ArgReader f
    | Just result <- f arg
    -> Just $ \args -> return (result, args)
  CmdReader _ f
    | Just cmdInfo <- f arg
    -> Just $ \args -> tryP $ runParser (infoParser cmdInfo) args
  _ -> Nothing
  where
    parsed
      | '-' : '-' : arg1 <- arg
      = case span (/= '=') arg1 of
          (_, "")           -> Just (OptLong arg1, Nothing)
          (arg1', _ : rest) -> Just (OptLong arg1', Just rest)
      | '-' : arg1 <- arg
      = case arg1 of
          []         -> Nothing
          [a]        -> Just (OptShort a, Nothing)
          (a : rest) -> Just (OptShort a, Just rest)
      | otherwise = Nothing

tryP :: Maybe a -> P a
tryP = maybe ParseError return

stepParser :: Parser a -> String -> [String] -> P (Parser a, [String])
stepParser (NilP _) _ _ = ParseError
stepParser (ConsP opt p) arg args
  | Just matcher <- optMatches (opt ^. the @"main") arg
  = do (r, args') <- matcher args
       liftOpt' <- tryP $ (opt ^. the @"cont") r
       return (liftOpt' <*> p, args')
  | otherwise
  = do (p', args') <- stepParser p arg args
       return (ConsP opt p', args')

runParser :: Parser a -> [String] -> Maybe (a, [String])
runParser p args = case args of
  [] -> result
  (arg : argt) -> case stepParser p arg argt of
    ParseError              -> result
    ParseResult (p', args') -> runParser p' args'
  where
    result = (,) <$> evalParser p <*> pure args

evalParser :: Parser a -> Maybe a
evalParser (NilP r)      = pure r
evalParser (ConsP opt p) = opt ^. the @"def" <*> evalParser p

mapParser :: (forall r x . Option r x -> b)
          -> Parser a
          -> [b]
mapParser _ (NilP _)      = []
mapParser f (ConsP opt p) = f opt : mapParser f p
