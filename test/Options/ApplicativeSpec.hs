module Options.ApplicativeSpec
  ( spec
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Extra
import Options.Applicative.Help
import Options.Applicative.Utils
import Test.Hspec

newtype HelloOptions = HelloOptions
  { hello :: String
  }

helloParser :: Parser HelloOptions
helloParser = HelloOptions
  <$> strOption
      ( long "hello"
      & metavar "TARGET"
      & help "Target for the greeting"
      )

helloHelp :: String
helloHelp = "hello - a test for optparse-applicative\
  \\n\
  \\nUsage: hello --hello TARGET\
  \\n  Print a greeting for TARGET\
  \\n\
  \\nCommon options:\
  \\n  -h,--help                Show this help text\
  \\n  --hello TARGET           Target for the greeting\
  \\n\
  \\n"

spec :: Spec
spec = describe "Options.ApplicativeSpec" $
  it "helloParser" $ requireTest $ do
    let opts = (info $ helper <*> helloParser)
          { infoFullDesc = True
          , infoProgDesc = "Print a greeting for TARGET"
          , infoHeader = vcat ["hello - a test for optparse-applicative", usage helloParser "hello"]
          }

    parserHelpText opts === helloHelp
