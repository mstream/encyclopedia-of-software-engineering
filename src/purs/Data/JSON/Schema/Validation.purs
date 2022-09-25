module Data.JSON.Schema.Validation (validate) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.JSON.Schema.Core (ArraySchemaFields, BooleanSchemaFields, NullSchemaFields, NumberSchemaFields, Schema(..), StringSchemaFields, ObjectSchemaFields)

type ValidationError = String

validate :: Schema -> Json -> Array ValidationError
validate schema json = case schema of
  ArraySchema fields ->
    validateArray fields json

  BooleanSchema fields ->
    validateBoolean fields json

  NullSchema fields ->
    validateNull fields json

  NumberSchema fields ->
    validateNumber fields json

  ObjectSchema fields ->
    validateObject fields json

  StringSchema fields ->
    validateString fields json

validateArray :: Record ArraySchemaFields -> Json -> Array ValidationError
validateArray _ = A.caseJsonArray
  [ "not an array value" ]
  (const [])

validateBoolean :: Record BooleanSchemaFields -> Json -> Array ValidationError
validateBoolean _ = A.caseJsonBoolean
  [ "not a boolean value" ]
  (const [])

validateNull :: Record NullSchemaFields -> Json -> Array ValidationError
validateNull _ = A.caseJsonNull
  [ "not a null value" ]
  (const [])

validateNumber :: Record NumberSchemaFields -> Json -> Array ValidationError
validateNumber _ = A.caseJsonNumber
  [ "not a number value" ]
  (const [])

validateObject :: Record ObjectSchemaFields -> Json -> Array ValidationError
validateObject _ = A.caseJsonObject
  [ "not an object value" ]
  (const [])

validateString :: Record StringSchemaFields -> Json -> Array ValidationError
validateString _ = A.caseJsonString
  [ "not a string value" ]
  (const [])
