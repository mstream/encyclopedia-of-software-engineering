module Data.Article (Article, Overview, Section) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Foldable (class Foldable)
import Data.Maybe (fromJust)
import Data.NonEmpty (NonEmpty)
import Data.Paragraph (Paragraph)
import Data.SandboxId (SandboxId)
import Data.Set (Set)
import Data.String.NonEmpty (NonEmptyString)
import Data.Tag (Tag)
import Partial.Unsafe (unsafePartial)

type Article =
  { overview ∷ Overview
  , sandboxes ∷ Set SandboxId
  , sections ∷ Array Section
  }

type Overview = NonEmptyArray Paragraph

type Section =
  { paragraphs ∷ NonEmptyArray Paragraph, title ∷ NonEmptyString }

