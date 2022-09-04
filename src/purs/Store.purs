module Store where

import Prelude

import Data.Maybe (Maybe(..))

type Store = {}

type Action = Unit

reduce :: Store -> Action -> Store
reduce store action = store

