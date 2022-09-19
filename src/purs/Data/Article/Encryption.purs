module Data.Article.Encryption (article) where


import Data.ArticleId (ArticleId(..))
import Data.Article (Article, Section, Overview)
import Data.Article as Article
import Data.Paragraph (Segment(..))
import Data.Paragraph as Paragraph
import Data.Set as Set
import Data.String.NonEmpty as NEString
import Data.Tag (Tag(..))
import Type.Proxy (Proxy(..))

article :: Article
article =
  { overview
  , sandboxes: []
  , sections
  , tags: Set.fromFoldable [ Cryptography ]
  }

overview :: Overview
overview =
  Article.unsafeOverview
    [ symmetricEncryptionParagraph
    , asymmetricEncryptionParagraph
    ]

  where
  asymmetricEncryptionParagraph = Paragraph.unsafeParagraph
    [ InternalReference
        (NEString.nes (Proxy :: _ "check out asymmetric encryption"))
        AsymmetricEncryption
    ]

  symmetricEncryptionParagraph = Paragraph.unsafeParagraph
    [ InternalReference
        (NEString.nes (Proxy :: _ "check out symmetric encryption"))
        SymmetricEncryption
    ]

sections :: Array Section
sections = []
