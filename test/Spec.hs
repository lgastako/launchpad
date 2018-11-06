
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

  context "colorVelocity" $ do

    it "should produce the same values as old clojure tests" $ do

      colorVelocity (R 0, G 0) `shouldBe` Velocity 0
      colorVelocity (R 3, G 0) `shouldBe` Velocity 3
      colorVelocity (R 0, G 1) `shouldBe` Velocity 16
      colorVelocity (R 2, G 2) `shouldBe` Velocity 34

  context "notePoint" $ do

    it "should produce the same values as old clojure tests" $ do

      notePoint (Note 0) `shouldBe`  (X 0, Y 0)
      notePoint (Note 1) `shouldBe`  (X 1, Y 0)
      notePoint (Note 16) `shouldBe` (X 0, Y 1)
      notePoint (Note 17) `shouldBe` (X 1, Y 1)
      notePoint (Note 32) `shouldBe` (X 0, Y 2)
      notePoint (Note 34) `shouldBe` (X 2, Y 2)

  context "pointNote" $ do

    it "should produce the same values as old clojure tests" $ do
      pointNote (X 0, Y 0) `shouldBe` Note 0
      pointNote (X 1, Y 0) `shouldBe` Note 1
      pointNote (X 0, Y 1) `shouldBe` Note 16
      pointNote (X 1, Y 1) `shouldBe` Note 17
      pointNote (X 0, Y 2) `shouldBe` Note 32
      pointNote (X 2, Y 2) `shouldBe` Note 34
