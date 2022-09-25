module Store where

import Prelude

type Store = {}

type Action = Unit

reduce :: Store -> Action -> Store
reduce store _ = store

