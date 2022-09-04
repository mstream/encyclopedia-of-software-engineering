module Data.Article.Encryption (article) where

import Data.Array.NonEmpty as NEArray
import Data.Article (Article)
import Data.Set as Set
import Data.String.NonEmpty as NEString
import Data.Tag (Tag(..))
import Type.Proxy (Proxy(..))

article :: Article
article =
  { overview: NEArray.singleton "TODO"
  , tags: Set.fromFoldable [ Cryptography ]
  , title: NEString.nes (Proxy :: _ "Encryption")
  }
