module Data.JSON.Schema.Core
  ( ArraySchemaFields
  , BooleanSchemaFields
  , CommonSchemaFields
  , NullSchemaFields
  , NumberSchemaFields
  , ObjectSchemaFields
  , StringSchemaFields
  , Schema(..)
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object as Object

data Schema
  = ArraySchema (Record ArraySchemaFields)
  | BooleanSchema (Record BooleanSchemaFields)
  | NullSchema (Record NullSchemaFields)
  | NumberSchema (Record NumberSchemaFields)
  | ObjectSchema (Record ObjectSchemaFields)
  | StringSchema (Record StringSchemaFields)

instance EncodeJson Schema where
  encodeJson = A.fromObject <<< Object.fromFoldable <<< case _ of
    ArraySchema fields ->
      [ "items" /\ encodeJson fields.items ]
        <> commonSchemaFieldsKeyValuePairs fields
        <> [ "type" /\ A.fromString "array" ]

    BooleanSchema fields ->
      commonSchemaFieldsKeyValuePairs fields <> [ "type" /\ A.fromString "boolean" ]

    NullSchema fields ->
      commonSchemaFieldsKeyValuePairs fields <> [ "type" /\ A.fromString "null" ]

    NumberSchema fields ->
      commonSchemaFieldsKeyValuePairs fields <> [ "type" /\ A.fromString "number" ]

    ObjectSchema fields ->
      commonSchemaFieldsKeyValuePairs fields <> [ "type" /\ A.fromString "object" ]

    StringSchema fields ->
      commonSchemaFieldsKeyValuePairs fields <> [ "type" /\ A.fromString "string" ]

commonSchemaFieldsKeyValuePairs
  :: forall specific
   . Record (CommonSchemaFields specific)
  -> Array (String /\ Json)
commonSchemaFieldsKeyValuePairs { required, title } =
  [ "required" /\ A.fromBoolean required ] <> case title of
    Just s ->
      [ "title" /\ A.fromString s ]

    Nothing ->
      []

type ArraySchemaFields = CommonSchemaFields (items :: Schema)
type BooleanSchemaFields = CommonSchemaFields ()
type NullSchemaFields = CommonSchemaFields ()
type NumberSchemaFields = CommonSchemaFields ()
type ObjectSchemaFields = CommonSchemaFields ()
type StringSchemaFields = CommonSchemaFields ()

type CommonSchemaFields specific = (required :: Boolean, title :: Maybe String | specific)

