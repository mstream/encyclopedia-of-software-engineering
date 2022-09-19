module Test.Spec.Data.JSON.Schema.Validation (spec) where

import Prelude

import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Data.Argonaut.Core as A
import Data.JSON.Schema.Core (Schema(..))
import Data.JSON.Schema.Validation (validate)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = do
  describe "Data.JSON.Schema.Validation" do
    describe "validate" do
      it "works for booleans" do
        quickCheck \boolean →
          let
            schema = BooleanSchema { required: true, title: Nothing }
            json = A.fromBoolean boolean
            actual = validate schema json
            expected = []

          in
            actual === expected

      it "works for nulls" do
        let
          schema = NullSchema { required: true, title: Nothing }
          json = A.jsonNull
          actual = validate schema json
          expected = []

        actual `shouldEqual` expected

      it "works for numbers" do
        quickCheck \number →
          let
            schema = NumberSchema { required: true, title: Nothing }
            json = A.fromNumber number
            actual = validate schema json
            expected = []

          in
            actual === expected

      it "works for strings" do
        quickCheck \string →
          let
            schema = StringSchema { required: true, title: Nothing }
            json = A.fromString string
            actual = validate schema json
            expected = []

          in
            actual === expected

