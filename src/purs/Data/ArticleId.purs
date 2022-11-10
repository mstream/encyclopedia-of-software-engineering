module Data.ArticleId
  ( ArticleId(..)
  , Title
  , codec
  , toNonEmptyString
  , toTitle
  , toString
  ) where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Codec (BasicCodec, basicCodec)
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Slug (Slug)
import Slug as Slug
import Type.Proxy (Proxy(..))
import Utils (allValues)

data ArticleId
  = AsymmetricEncryption
  | BasicHttpAuthentication
  | BubbleSort
  | CryptographicHashing
  | Encryption
  | SymmetricEncryption

derive instance Generic ArticleId _

derive instance Eq ArticleId

derive instance Ord ArticleId

instance Enum ArticleId where
  pred = genericPred
  succ = genericSucc

instance Bounded ArticleId where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum ArticleId where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

newtype Title = Title NonEmptyString

derive newtype instance Eq Title
derive newtype instance Ord Title

toString ∷ Title → String
toString = NEString.toString <<< toNonEmptyString

toNonEmptyString ∷ Title → NonEmptyString
toNonEmptyString (Title nes) = nes

toTitle ∷ ArticleId → Title
toTitle = Title <<< case _ of
  AsymmetricEncryption →
    NEString.nes (Proxy ∷ _ "Asymmetric Encryption")

  BasicHttpAuthentication →
    NEString.nes (Proxy ∷ _ "Basic HTTP Authentication")

  BubbleSort →
    NEString.nes (Proxy ∷ _ "Bubble Sort")

  CryptographicHashing →
    NEString.nes (Proxy ∷ _ "Cryptographic Hashing")

  Encryption →
    NEString.nes (Proxy ∷ _ "Encryption")

  SymmetricEncryption →
    NEString.nes (Proxy ∷ _ "Symmetric Encryption")

toSlug ∷ ArticleId → Slug
toSlug = unsafePartial $ fromJust
  <<< Slug.generate
  <<< toString
  <<< toTitle

encode ∷ ArticleId → String
encode = Slug.toString <<< toSlug

decode ∷ String → String \/ ArticleId
decode = Slug.parse >>> case _ of
  Nothing →
    Left "invalid slug"

  Just slug →
    note "article not found" $ Map.lookup slug idsBySlug

  where
  idsBySlug = Map.fromFoldable
    $ (\id → toSlug id /\ id) <$> (allValues ∷ Array ArticleId)

codec ∷ BasicCodec (Either String) String ArticleId
codec = basicCodec decode encode
