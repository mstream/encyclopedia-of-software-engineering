module Test.Spec.Data (spec) where

import Prelude

import Test.Spec (Spec, describe)
import Test.Spec.Data.CesarCypher as CesarCypher
import Test.Spec.Data.HashCollision as HashCollision
import Test.Spec.Data.JSON.Schema.Core as JsonSchemaCore
import Test.Spec.Data.JSON.Schema.Validation as JsonSchemaValidation
import Test.Spec.Data.Program as Program
import Test.Spec.Data.Sorting as Sorting

spec ∷ Spec Unit
spec = do
  describe "Data" do
    CesarCypher.spec
    HashCollision.spec
    JsonSchemaCore.spec
    JsonSchemaValidation.spec
    Program.spec
    Sorting.spec

