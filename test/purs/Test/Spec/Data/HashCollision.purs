module Test.Spec.Data.HashCollision (spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.QuickCheck ((===))
import Test.Spec.QuickCheck (quickCheck)
import Data.HashCollision as HashCollision
import Data.List as List

spec ∷ Spec Unit
spec = do
  describe "Data.HashCollision" do
    describe "digest" do
      it "produces a fix-length output for the same algorithm" do
        quickCheck \algorithm input →
          let
            actual = List.length $ HashCollision.digest algorithm input

            expected = List.length
              $ HashCollision.digest algorithm
              $ input <> input

          in
            actual === expected

