module Data.Article.BasicHttpAuthentication (article) where

import Data.Article (Article, Overview, Section)
import Data.Article as Article
import Data.Paragraph (Segment(..))
import Data.Paragraph as Paragraph
import Data.SandboxId (SandboxId(..))
import Data.Set as Set

article ∷ Article
article =
  { overview
  , sandboxes: Set.fromFoldable [ HttpBasicAuth ]
  , sections
  }

overview ∷ Overview
overview = Article.unsafeOverview
  [ Paragraph.unsafeParagraph [ Text "TODO(Overview)" ] ]

sections ∷ Array Section
sections =
  []

