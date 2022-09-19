module Test.Spec.Data.JSON.Schema.Core (spec) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode (encodeJson)
import Data.JSON.Schema.Core (Schema(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = do
  describe "Data.JSON.Schema.Core" do
    describe "Schema" do
      describe "encodeJson" do
        it "works for arrays" do
          let
            itemsSchema = BooleanSchema { required: true, title: Nothing }

            actual = encodeJson $ ArraySchema
              { items: itemsSchema
              , required: true
              , title: Just "an array value"
              }

            expected = A.fromObject
              $ Object.fromFoldable
                  [ "items" /\ encodeJson itemsSchema
                  , "required" /\ A.fromBoolean true
                  , "title" /\ A.fromString "an array value"
                  , "type" /\ A.fromString "array"
                  ]

          A.stringify actual `shouldEqual` A.stringify expected

        it "works for nulls" do
          let
            actual = encodeJson $ NullSchema
              { required: true
              , title: Just "a null value"
              }

            expected = A.fromObject
              $ Object.fromFoldable
                  [ "required" /\ A.fromBoolean true
                  , "title" /\ A.fromString "a null value"
                  , "type" /\ A.fromString "null"
                  ]

          A.stringify actual `shouldEqual` A.stringify expected

        it "works for booleans" do
          let
            actual = encodeJson $ BooleanSchema
              { required: true
              , title: Just "a boolean value"
              }

            expected = A.fromObject
              $ Object.fromFoldable
                  [ "required" /\ A.fromBoolean true
                  , "title" /\ A.fromString "a boolean value"
                  , "type" /\ A.fromString "boolean"
                  ]

          A.stringify actual `shouldEqual` A.stringify expected

        it "works for numbers" do
          let
            actual = encodeJson $ NumberSchema
              { required: true
              , title: Just "a number value"
              }

            expected = A.fromObject
              $ Object.fromFoldable
                  [ "required" /\ A.fromBoolean true
                  , "title" /\ A.fromString "a number value"
                  , "type" /\ A.fromString "number"
                  ]

          A.stringify actual `shouldEqual` A.stringify expected

        it "works for objects" do
          let
            actual = encodeJson $ ObjectSchema
              { required: true
              , title: Just "an object value"
              }

            expected = A.fromObject
              $ Object.fromFoldable
                  [ "required" /\ A.fromBoolean true
                  , "title" /\ A.fromString "an object value"
                  , "type" /\ A.fromString "object"
                  ]

          A.stringify actual `shouldEqual` A.stringify expected

        it "works for strings" do
          let
            actual = encodeJson $ StringSchema
              { required: true
              , title: Just "a string value"
              }

            expected = A.fromObject
              $ Object.fromFoldable
                  [ "required" /\ A.fromBoolean true
                  , "title" /\ A.fromString "a string value"
                  , "type" /\ A.fromString "string"
                  ]

          A.stringify actual `shouldEqual` A.stringify expected

