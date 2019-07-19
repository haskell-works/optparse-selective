import Control.Applicative
import Control.Selective
import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Extra
import Options.Applicative.Types

data Test = Test
  { foo :: Either String Int
  , moo :: String
  }
  deriving Show

parser :: Parser Test
parser =
  Test
    <$> NilP (Right 1)
    <*> (NilP (Left "moo") <*? NilP (\moo -> "goo"))

blah :: Test -> IO ()
blah _ = putStrLn "blah"

main :: IO ()
main = execParser opts >>= blah
  where
    opts = (info $ helper <*> parser)
      { infoFullDesc = True
      , infoProgDesc = "Print a greeting for TARGET"
      , infoHeader = "hello - a test for optparse-applicative" }
