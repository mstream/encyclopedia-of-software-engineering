module Data.HashCollision
  ( Algorithm(..)
  , Config(..)
  , algorithm
  , defaultAlgorithm
  , digest
  , input
  , maxInputLength
  , minInputLength
  , toString
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExcept)
import Data.Binary (Bit(..), Byte)
import Data.Binary as Binary
import Data.Either (Either(..), hush)
import Data.Either.Nested (type (\/))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Foreign (Foreign, ForeignError(..))
import Foreign as F
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

type Config =
  { algorithm ∷ Algorithm, input1 ∷ String, input2 ∷ String }

data Algorithm = Md5 | Sha1 | Sha256 | Sha512

derive instance Generic Algorithm _
derive instance Eq Algorithm

instance Arbitrary Algorithm where
  arbitrary = genericArbitrary

minInputLength ∷ Int
minInputLength = 0

maxInputLength ∷ Int
maxInputLength = 16

defaultAlgorithm ∷ Algorithm
defaultAlgorithm = Md5

algorithm ∷ String → String \/ Algorithm
algorithm = case _ of
  "MD5" → Right Md5
  "SHA-1" → Right Sha1
  "SHA-256" → Right Sha256
  "SHA-512" → Right Sha512
  _ → Left "unknown algorithm"

input ∷ String → String \/ String
input = Right

toString ∷ Algorithm → String
toString = case _ of
  Md5 →
    "MD5"

  Sha1 →
    "SHA-1"

  Sha256 →
    "SHA-256"

  Sha512 →
    "SHA-512"

digest ∷ Algorithm → String → List Byte
digest hashAlgorithm inputString = unsafePartial $ fromJust do
  bits ← List.fromFoldable <$> hush bitsResult

  if List.length bits `mod` 8 /= 0 then Nothing
  else Just $ bits # unfoldr \remainingBits → do
    byte ← Binary.fromList $ List.take 8 remainingBits
    pure $ byte /\ List.drop 8 remainingBits

  where
  bitsResult = runExcept do
    values ← F.readArray
      $ runFn2 digestImpl (toString hashAlgorithm) inputString

    traverse readBit values

readBit
  ∷ ∀ m
  . Monad m
  ⇒ Foreign
  → ExceptT (NonEmptyList ForeignError) m Bit
readBit value = do
  s ← F.readString value
  case s of
    "0" →
      pure Zero

    "1" →
      pure One

    otherString →
      F.fail $ ForeignError $ "'" <> otherString <> "' is not a bit"

foreign import digestImpl ∷ Fn2 String String Foreign
