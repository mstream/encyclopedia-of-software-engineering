module Data.Program
  ( Event(..)
  , ProgramError(..)
  , ProgramF
  , ProgramResult
  , compareInputElements
  , decrementVariable
  , decrementVariable_
  , getInputElementsNumber
  , getVariable
  , incrementVariable
  , incrementVariable_
  , print
  , executeProgram
  , for
  , setVariable
  , swapInputElements
  , updateVariable
  , updateVariable_
  , while
  ) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Either (either, note)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Run (Run)
import Run as Run
import Run.Except (EXCEPT, runExcept, throw)
import Run.State (STATE, execState, get, modify)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data ProgramF a
  = Print String a
  | CompareInputElements Int Int (Ordering → a)
  | CompareVariables String String (Ordering → a)
  | GetInputElementsNumber (Int → a)
  | GetVariable String (Int → a)
  | SetVariable String Int a
  | SwapInputElements Int Int a

derive instance Functor ProgramF

type PROGRAM r = (program ∷ ProgramF | r)

_program ∷ Proxy "program"
_program = Proxy

print ∷ ∀ r. String → Run (PROGRAM + r) Unit
print s = Run.lift _program $ Print s unit

compareInputElements ∷ ∀ r. Int → Int → Run (PROGRAM + r) Ordering
compareInputElements idx1 idx2 = Run.lift _program
  $ CompareInputElements idx1 idx2 identity

getInputElementsNumber ∷ ∀ r. Run (PROGRAM + r) Int
getInputElementsNumber = Run.lift _program
  $ GetInputElementsNumber identity

getVariable ∷ ∀ r. String → Run (PROGRAM + r) Int
getVariable name = Run.lift _program
  $ GetVariable name identity

decrementVariable_ ∷ ∀ r. String → Run (PROGRAM + r) Unit
decrementVariable_ = void <<< decrementVariable

decrementVariable ∷ ∀ r. String → Run (PROGRAM + r) Int
decrementVariable = updateVariable (_ - 1)

incrementVariable_ ∷ ∀ r. String → Run (PROGRAM + r) Unit
incrementVariable_ = void <<< incrementVariable

incrementVariable ∷ ∀ r. String → Run (PROGRAM + r) Int
incrementVariable = updateVariable (_ + 1)

updateVariable_ ∷ ∀ r. (Int → Int) → String → Run (PROGRAM + r) Unit
updateVariable_ f name = void $ updateVariable f name

updateVariable ∷ ∀ r. (Int → Int) → String → Run (PROGRAM + r) Int
updateVariable f name = do
  value ← getVariable name

  let
    newValue = f value

  setVariable name newValue
  pure newValue

setVariable ∷ ∀ r. String → Int → Run (PROGRAM + r) Unit
setVariable name value = Run.lift _program
  $ SetVariable name value unit

swapInputElements ∷ ∀ r. Int → Int → Run (PROGRAM + r) Unit
swapInputElements idx1 idx2 = Run.lift _program
  $ SwapInputElements idx1 idx2 unit

for
  ∷ ∀ r
  . Run (PROGRAM + r) Unit
  → Run (PROGRAM + r) Boolean
  → Run (PROGRAM + r) Unit
  → Run (PROGRAM + r) Unit
  → Run (PROGRAM + r) Unit
for init condition iteration program = do
  init
  while condition do
    program
    iteration

while
  ∷ ∀ r
  . Run (PROGRAM + r) Boolean
  → Run (PROGRAM + r) Unit
  → Run (PROGRAM + r) Unit
while condition program = do
  shouldContinue ← condition
  if shouldContinue then do
    program
    while condition program
  else pure unit

data Event
  = Printed String
  | InputElementsCompared Int Int Ordering
  | InputElementsSwapped Int Int
  | VariableSet String Int

derive instance Generic Event _
derive instance Eq Event

instance Show Event where
  show = genericShow

type ProgramState =
  { events ∷ List Event, input ∷ Array Int, variables ∷ Map String Int }

data ProgramError = IndexOutOfBound Int Int | UndefinedVariable String

derive instance Generic ProgramError _

derive instance Eq ProgramError

instance Show ProgramError where
  show = genericShow

type ProgramResult = { events ∷ List Event, output ∷ Array Int }

executeProgram
  ∷ ∀ a
  . Run (EXCEPT ProgramError + PROGRAM + STATE ProgramState + ()) a
  → Array Int
  → ProgramError \/ ProgramResult
executeProgram program input = do
  result ← programResult
  pure { events: List.reverse result.events, output: result.input }
  where
  programResult = Run.extract $ program
    # runProgram
    # execState
        { events: Nil
        , input
        , variables: Map.empty
        }
    # runExcept

runProgram
  ∷ ∀ r
  . Run (EXCEPT ProgramError + STATE ProgramState + PROGRAM + r)
      ~> Run (EXCEPT ProgramError + STATE ProgramState + r)
runProgram = Run.interpret (Run.on _program handleProgram Run.send)

handleProgram
  ∷ ∀ r
  . ProgramF ~> Run (EXCEPT ProgramError + STATE ProgramState + r)
handleProgram = case _ of
  CompareInputElements idx1 idx2 reply → do
    { input } ← get
    either
      (throw <<< IndexOutOfBound (Array.length input))
      ( \ordering → do
          modify \st → st
            { events = InputElementsCompared idx1 idx2 ordering :
                st.events
            }
          pure $ reply ordering
      )
      do
        val1 ← note idx1 $ input !! idx1
        val2 ← note idx2 $ input !! idx2
        pure $ val1 `compare` val2

  CompareVariables name1 name2 reply → do
    { variables } ← get
    either
      (throw <<< UndefinedVariable)
      (pure <<< reply)
      do
        val1 ← note name1 $ Map.lookup name1 variables
        val2 ← note name2 $ Map.lookup name2 variables
        pure $ val1 `compare` val2

  GetInputElementsNumber reply → do
    { input } ← get
    pure $ reply $ Array.length input

  GetVariable name reply → do
    { variables } ← get
    maybe
      (throw $ UndefinedVariable name)
      (pure <<< reply)
      (Map.lookup name variables)

  Print s next → do
    modify \st →
      st { events = Printed s : st.events }

    pure next

  SetVariable name value next → do
    modify \st →
      st
        { events = VariableSet name value : st.events
        , variables = Map.insert name value st.variables
        }

    pure next

  SwapInputElements idx1 idx2 next → do
    { input } ← get
    either
      (throw <<< IndexOutOfBound (Array.length input))
      ( \newInput → modify \st → st
          { events = InputElementsSwapped idx1 idx2 : st.events
          , input = newInput
          }
      )
      do
        val1 ← note idx1 $ input !! idx1
        val2 ← note idx2 $ input !! idx2

        pure
          $ Array.updateAtIndices [ idx1 /\ val2, idx2 /\ val1 ] input

    pure next
