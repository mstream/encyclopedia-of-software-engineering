module Data.Tag (Tag(..)) where

import Prelude

data Tag = Cryptography

derive instance Eq Tag
derive instance Ord Tag

