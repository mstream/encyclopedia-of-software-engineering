module Utils (allValues) where

import Prelude

import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Unfoldable (class Unfoldable1)

allValues :: forall a u. BoundedEnum a => Unfoldable1 u => u a
allValues = upFromIncluding bottom
