module Data.Article.Encryption (article) where

import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Article (Article, Overview, Section)
import Data.ArticleId (ArticleId(..))
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
        (Proxy ∷ _ "A process of encoding information.")
    )
      :|
        [ ( Text $ NEString.nes
              ( Proxy
                  ∷ _
                      "This process converts the original representation of the information, known as plaintext, into an alternative form known as ciphertext."
              )
          )
        , ( Text $ NEString.nes
              ( Proxy
                  ∷ _
                      "Ideally, only authorized parties can decipher a ciphertext back to plaintext and access the original information. Encryption does not itself prevent interference but denies the intelligible content to a would-be interceptor."
              )
          )
        ]

sections ∷ Array Section
sections = [ typesSection ]

typesSection ∷ Section
typesSection =
  { paragraphs: NEArray.fromNonEmpty $ symmetricEncryptionParagraph
      :| [ asymmetricEncryptionParagraph ]
  , title: NEString.nes (Proxy ∷ _ "Types")
  }
  where
  asymmetricEncryptionParagraph = NEArray.fromNonEmpty $
    InternalReference
      (NEString.nes (Proxy ∷ _ "check out asymmetric encryption"))
      AsymmetricEncryption :| []

  symmetricEncryptionParagraph = NEArray.fromNonEmpty $
    InternalReference
      (NEString.nes (Proxy ∷ _ "check out symmetric encryption"))
      SymmetricEncryption :| []

