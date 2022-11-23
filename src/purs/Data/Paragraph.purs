module Data.Paragraph
  ( ExternalLink(..)
  , Paragraph
  , Segment(..)
  , toString
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.ArticleId (ArticleId)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString

type Paragraph = NonEmptyArray Segment

data Segment
  = ExternalReference NonEmptyString ExternalLink
  | InternalReference NonEmptyString ArticleId
  | Text NonEmptyString

data ExternalLink = Wikipedia NonEmptyString

toString ∷ ExternalLink → String
toString = case _ of
  Wikipedia slug →
    "https://en.wikipedia.org/wiki/" <> NEString.toString slug

