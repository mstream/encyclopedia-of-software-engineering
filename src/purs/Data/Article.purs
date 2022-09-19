module Data.Article (Article, Overview, Section, Tags, unsafeOverview) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Array.NonEmpty as NEArray
import Data.Paragraph (Paragraph)
import Data.Set (Set)
import Data.String.NonEmpty (NonEmptyString)
import Data.Tag (Tag)
import Effect.Aff (Aff)
import Component.Sandbox (SandboxComponent)

type Article =
  { overview :: Overview
  , sandboxes :: Array (SandboxComponent Aff)
  , sections :: Array Section
  , tags :: Tags
  }

type Overview = NonEmptyArray Paragraph

type Section = { paragraphs :: NonEmptyArray Paragraph, title :: NonEmptyString }

type Tags = Set Tag

unsafeOverview :: forall f. Foldable f => f Paragraph -> Overview
unsafeOverview = unsafePartial $ fromJust <<< NEArray.fromFoldable
