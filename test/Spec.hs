
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Launchpad.Prelude

import Launchpad
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< testGroup "Specs" <$> mapM (uncurry testSpec)
  [ ("All", launchpadSpec)
  ]

launchpadSpec :: Spec
launchpadSpec = describe "Launchpad" $ do

  it "1 + 1 = 2" $
    1 + 1 `shouldBe` (2 :: Int)
