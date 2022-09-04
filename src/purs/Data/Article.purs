module Data.Article (Article, Overview, Title, Tags, codec) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Codec (BasicCodec, basicCodec)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.Paragraph (Paragraph)
import Data.Set (Set)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tag (Tag)
import Partial.Unsafe (unsafePartial)
import Slug (Slug)
import Slug as Slug

type Article = { overview :: Overview, title :: Title, tags :: Tags }
type Overview = NonEmptyArray Paragraph
type Title = NonEmptyString
type Tags = Set Tag

codec :: Set Article -> BasicCodec (Either String) String Article
codec articles = basicCodec decode encode
  where
  encode = Slug.toString <<< slug
  decode = Slug.parse >>> case _ of
    Nothing ->
      Left "invalid slug"

    Just otherSlug ->
      foldl
        ( \acc article ->
            if slug article == otherSlug then Right article
            else acc
        )
        (Left "non-existent article")
        articles

slug :: Article -> Slug
slug { title } =
  unsafePartial $ fromJust $ Slug.generate $ NEString.toString title
