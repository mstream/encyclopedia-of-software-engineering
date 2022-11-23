module Data.Article.CryptographicHashing (article) where

import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Article (Article, Overview, Section)
import Data.NonEmpty ((:|))
import Data.Paragraph (Segment(..))
import Data.SandboxId (SandboxId(..))
import Data.Set as Set
import Data.String.NonEmpty as NEString
import Type.Proxy (Proxy(..))

article ∷ Article
article =
  { overview
  , sandboxes: Set.fromFoldable [ HashCollision ]
  , sections
  }

overview ∷ Overview
overview = NEArray.fromNonEmpty $ summaryParagraph :| []
  where
  summaryParagraph =
    NEArray.fromNonEmpty $
      ( Text $ NEString.nes
          ( Proxy
              ∷ _
                  "A set of hashing algorithms, for which for is practically infeasible to invert or reverse the computation."
          )
      ) :| []

sections ∷ Array Section
sections = [ typesSection ]

typesSection ∷ Section
typesSection =
  { paragraphs: NEArray.fromNonEmpty $ typesParagraph :| []
  , title: NEString.nes (Proxy ∷ _ "Types")
  }
  where
  typesParagraph = NEArray.fromNonEmpty $
    (Text $ NEString.nes (Proxy ∷ _ "TODO(Types)")) :| []
