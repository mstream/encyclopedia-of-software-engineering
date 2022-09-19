module Test.Spec.Data.CesarCypher (spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.QuickCheck ((===))
import Test.Spec.QuickCheck (quickCheck)
import Data.CesarCypher as CesarCypher

spec ∷ Spec Unit
spec = do
  describe "Data.CesarCypher" do
    describe "Character" do
      it "maintains encryption and decryption symmetry" do
        quickCheck \c k →
          let
            actual = (CesarCypher.decrypt k <<< CesarCypher.encrypt k) c
            expected = c
          in
            actual === expected

