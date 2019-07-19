module Options.ApplicativeSpec
  ( spec
  ) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

spec :: Spec
spec = describe "Options.ApplicativeSpec" $
  it "stub" $ requireTest $
    True === True
