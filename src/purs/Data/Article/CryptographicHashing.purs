module Data.Article.CryptographicHashing (article) where

import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Article (Article, Overview, Section)
import Data.Article as Article
import Data.Paragraph (Segment(..))
import Data.Paragraph as Paragraph
import Data.SandboxId (SandboxId(..))
import Data.Set as Set
import Data.String.NonEmpty as NEString
import Data.Tag (Tag(..))
import Type.Proxy (Proxy(..))

article ∷ Article
article =
  { overview
  , sandboxes: Set.fromFoldable [ HashCollision ]
  , sections
  , tags: Set.fromFoldable [ Cryptography ]
  }

overview ∷ Overview
overview = Article.unsafeOverview
  [ Paragraph.unsafeParagraph [ Text "TODO(Overview)" ] ]

sections ∷ Array Section
sections =
  [ typesSection
  ]

typesSection ∷ Section
typesSection =
  { paragraphs: NEArray.singleton $ Paragraph.unsafeParagraph
      [ Text "TODO(Types)" ]
  , title: NEString.nes (Proxy ∷ _ "Types")
  }
