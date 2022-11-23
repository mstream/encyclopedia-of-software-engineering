module Data.Article.SymmetricEncryption (article) where

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
  , sandboxes: Set.fromFoldable [ CesarCypher ]
  , sections
  }

overview ∷ Overview
overview = NEArray.fromNonEmpty $ summaryParagraph :| []
  where
  summaryParagraph = NEArray.fromNonEmpty $
    ( Text $ NEString.nes
        ( Proxy
            ∷ _
                "algorithms for cryptography that use the same cryptographic keys for both the encryption of plaintext and the decryption of ciphertext."
        )
    )
      :| []

sections ∷ Array Section
sections =
  [ typesSection
  ]

typesSection ∷ Section
typesSection =
  { paragraphs: NEArray.fromNonEmpty $ typesParagraph :| []
  , title: NEString.nes (Proxy ∷ _ "Types")
  }
  where
  typesParagraph = NEArray.fromNonEmpty $
    (Text $ NEString.nes (Proxy ∷ _ "TODO(Types)")) :| []

