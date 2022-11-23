module Data.Article.BasicHttpAuthentication (article) where

import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Article (Article, Overview, Section)
import Data.NonEmpty ((:|))
import Data.Paragraph (ExternalLink(..), Segment(..))
import Data.SandboxId (SandboxId(..))
import Data.Set as Set
import Data.String.NonEmpty as NEString
import Type.Proxy (Proxy(..))

article ∷ Article
article =
  { overview
  , sandboxes: Set.fromFoldable [ HttpBasicAuth ]
  , sections
  }

overview ∷ Overview
overview = NEArray.fromNonEmpty $ summaryParagraph :| []
  where
  summaryParagraph = NEArray.fromNonEmpty $
    (Text $ NEString.nes (Proxy ∷ _ "One of methods for a")) :|
      [ ExternalReference
          (NEString.nes (Proxy ∷ _ "HTTP user agent"))
          httpUserAgentLink
      , Text $ NEString.nes
          (Proxy ∷ _ "to provide user name and password in a request.")
      ]

sections ∷ Array Section
sections =
  []

httpUserAgentLink ∷ ExternalLink
httpUserAgentLink = Wikipedia $ NEString.nes (Proxy ∷ _ "User_agent")
