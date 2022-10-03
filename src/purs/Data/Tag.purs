module Data.Tag (Tag(..)) where

import Prelude

data Tag = Algorithms | Cryptography | Sorting

derive instance Eq Tag
derive instance Ord Tag

