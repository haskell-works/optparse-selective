module Options.Applicative.Extra (
  helper,
  execParser,
  execParser',
  usage
  ) where

import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Help
import Options.Applicative.Types   hiding (help)
import Options.Applicative.Utils
import System.Environment
import System.Exit
import System.IO

helper :: Parser (a -> a)
helper = nullOption
       ( long "help"
       & short 'h'
       & help "Show this help text"
       & value id
       & hide )

execParser' :: String -> [String] -> ParserInfo a -> IO a
execParser' prog args pinfo = do
  let p = infoParser pinfo
  case runParser p args of
    Just (a, []) -> return a
    _ -> do
      let pinfo' = pinfo
            { infoHeader = vcat [infoHeader pinfo, usage p prog] }
      hPutStr stderr $ parserHelpText pinfo'
      exitWith (ExitFailure 1)

execParser :: ParserInfo a -> IO a
execParser pinfo = do
  prog <- getProgName
  args <- getArgs
  execParser' prog args pinfo

usage :: Parser a -> String -> String
usage p prog = foldr (<+>) ""
  [ "Usage:"
  , prog
  , shortDesc p ]
