module Test.Spec.Data.Program (spec) where

import Prelude

import Data.Either (Either(..))
import Data.List as List
import Data.Program (Event(..), ProgramError(..))
import Data.Program as Program
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = do
  describe "Data.Program" do
    describe "evaluate" do
      it "produces expected events" do
        let
          program = do
            Program.print "hello"
            Program.setVariable "x" 3
            Program.print =<< show <$> Program.getVariable "x"

            Program.print =<< show <$> eq EQ <$>
              Program.compareInputElements 0 0

            Program.print =<< show <$> eq EQ <$>
              Program.compareInputElements 0 1

          actual = Program.executeProgram program [ 1, 2 ]

          expected = Right $
            { events: List.fromFoldable
                [ Printed "hello"
                , VariableSet "x" 3
                , Printed "3"
                , InputElementsCompared 0 0 EQ
                , Printed "true"
                , InputElementsCompared 0 1 LT
                , Printed "false"
                ]
            , output: [ 1, 2 ]
            }

        actual `shouldEqual` expected

      it "fails when accessing undefined variable" do
        let
          program = do
            Program.getVariable "y"

          actual = Program.executeProgram program []
          expected = Left $ UndefinedVariable "y"

        actual `shouldEqual` expected

      it "fails when comparing non-existent input elements" do
        let
          program = do
            Program.compareInputElements 0 1

          actual = Program.executeProgram program [ 1 ]
          expected = Left $ IndexOutOfBound 1 1

        actual `shouldEqual` expected

