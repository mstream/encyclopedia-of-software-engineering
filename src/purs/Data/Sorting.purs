module Data.Sorting
  ( Config(..)
  , bubble
  ) where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Program (ProgramError, ProgramResult)
import Data.Program as P
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))

type Config = Array Int

bubble ∷ Config → ProgramError \/ ProgramResult
bubble = P.executeProgram
  $ P.for
      (P.setVariable "i" =<< (_ - 1) <$> P.getInputElementsNumber)
      ((_ > 0) <$> P.getVariable "i")
      (P.decrementVariable_ "i")
  $ P.for
      (P.setVariable "j" 0)
      ( (\(i /\ j) → j < i)
          <$> (Tuple <$> P.getVariable "i" <*> P.getVariable "j")
      )
      (P.incrementVariable_ "j")
      do
        j ← P.getVariable "j"
        shouldSwap ← eq GT <$> P.compareInputElements j (j + 1)
        when shouldSwap $ P.swapInputElements j (j + 1)
