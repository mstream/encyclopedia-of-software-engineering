module Test.Spec.Data.Sorting (spec) where

import Prelude

import Data.Either (Either(..))
import Data.List as List
import Data.Program (Event(..))
import Data.Sorting as Sorting
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = do
  describe "Data.Sorting" do
    describe "evaluate" do
      it "produces expected events" do
        let
          actual = Sorting.bubble [ 1, 3, 2, 4 ]
          expected = Right $
            { events: List.fromFoldable
                [ VariableSet "i" 3
                , VariableSet "j" 0
                , InputElementsCompared 0 1 LT
                , VariableSet "j" 1
                , InputElementsCompared 1 2 GT
                , InputElementsSwapped 1 2
                , VariableSet "j" 2
                , InputElementsCompared 2 3 LT
                , VariableSet "j" 3
                , VariableSet "i" 2
                , VariableSet "j" 0
                , InputElementsCompared 0 1 LT
                , VariableSet "j" 1
                , InputElementsCompared 1 2 LT
                , VariableSet "j" 2
                , VariableSet "i" 1
                , VariableSet "j" 0
                , InputElementsCompared 0 1 LT
                , VariableSet "j" 1
                , VariableSet "i" 0
                ]
            , output: [ 1, 2, 3, 4 ]
            }

        actual `shouldEqual` expected

