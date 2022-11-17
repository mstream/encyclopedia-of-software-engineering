module Data.Tag (Tag(..), byArticleId, toString) where

import Prelude

import Data.ArticleId (ArticleId(..))
import Data.Set (Set)
import Data.Set as Set

data Tag = Algorithms | Cryptography | Sorting

derive instance Eq Tag
derive instance Ord Tag

byArticleId ∷ ArticleId → Set Tag
byArticleId = case _ of
  AsymmetricEncryption →
    Set.fromFoldable [ Algorithms, Cryptography ]

  BasicHttpAuthentication →
    Set.fromFoldable [ Cryptography ]

  BubbleSort →
    Set.fromFoldable [ Algorithms, Sorting ]

  CryptographicHashing →
    Set.fromFoldable [ Algorithms, Cryptography ]

  Encryption →
    Set.fromFoldable [ Cryptography ]

  SymmetricEncryption →
    Set.fromFoldable [ Algorithms, Cryptography ]

toString ∷ Tag → String
toString = case _ of
  Algorithms →
    "algorithms"

  Cryptography →
    "cryptography"

  Sorting →
    "sorting"
