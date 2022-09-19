module Data.Paragraph (Paragraph, Segment(..), unsafeParagraph) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.ArticleId (ArticleId)
import Data.Foldable (class Foldable)
import Data.Maybe (fromJust)
import Data.String.NonEmpty (NonEmptyString)
import Partial.Unsafe (unsafePartial)
import Data.Array.NonEmpty as NEArray

type Paragraph = NonEmptyArray Segment

data Segment
  = ExternalReference NonEmptyString String
  | InternalReference NonEmptyString ArticleId
  | Text String

unsafeParagraph :: forall f. Foldable f => f Segment -> Paragraph
unsafeParagraph = unsafePartial $ fromJust <<< NEArray.fromFoldable
