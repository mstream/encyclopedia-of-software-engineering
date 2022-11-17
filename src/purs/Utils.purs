module Utils (allValues, capitalize) where

import Prelude

import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.String as String
import Data.Unfoldable (class Unfoldable1)

allValues ∷ ∀ a u. BoundedEnum a ⇒ Unfoldable1 u ⇒ u a
allValues = upFromIncluding bottom

capitalize ∷ String → String
capitalize = String.splitAt 1 >>> \{ after, before } →
  String.toUpper before <> after
