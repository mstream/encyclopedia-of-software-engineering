module Data.Article.AsymmetricEncryption (article) where

import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Article (Article, Overview, Section)
import Data.NonEmpty ((:|))
import Data.Paragraph (Segment(..))
import Data.Set as Set
import Data.String.NonEmpty as NEString
import Type.Proxy (Proxy(..))

article ∷ Article
article =
  { overview
  , sandboxes: Set.fromFoldable []
  , sections
  }

overview ∷ Overview
overview = NEArray.fromNonEmpty $ summaryParagraph :| []
  where
  summaryParagraph = NEArray.fromNonEmpty $
    ( Text $ NEString.nes
        ( Proxy
            ∷ _
                "A field of cryptographic systems that use pairs of related keys."
        )
    ) :|
      [ ( Text $ NEString.nes
            ( Proxy
                ∷ _
                    "Each key pair consists of a public key and a corresponding private key"
            )
        )
      ]

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
