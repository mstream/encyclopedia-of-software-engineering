module Data.Article.BubbleSort (article) where

import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Article (Article, Overview, Section)
import Data.Article as Article
import Data.NonEmpty ((:|))
import Data.Paragraph (Segment(..))
import Data.Paragraph as Paragraph
import Data.SandboxId (SandboxId(..))
import Data.Set as Set
import Data.String.NonEmpty as NEString
import Type.Proxy (Proxy(..))

article ∷ Article
article =
  { overview
  , sandboxes: Set.fromFoldable [ BubbleSort ]
  , sections
  }

overview ∷ Overview
overview = NEArray.fromNonEmpty $ summaryParagraph :| []
  where
  summaryParagraph = NEArray.fromNonEmpty $
    ( Text $ NEString.nes
        ( Proxy
            ∷ _
                "A simple sorting algorithm that repeatedly steps through the input list element by element, comparing the current element with the one after it, swapping their values if needed."
        )
    ) :| []

sections ∷ Array Section
sections =
  [ complexitySection ]

complexitySection ∷ Section
complexitySection =
  { paragraphs: NEArray.fromNonEmpty $ complexityParagraph :| []
  , title: NEString.nes (Proxy ∷ _ "Complexity")
  }
  where
  complexityParagraph = NEArray.fromNonEmpty $
    (Text $ NEString.nes (Proxy ∷ _ "TODO(Complexity)")) :| []

