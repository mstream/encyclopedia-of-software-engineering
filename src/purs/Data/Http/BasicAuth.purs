module Data.Http.BasicAuth
  ( Config(..)
  , maxPasswordLength
  , maxUsernameLength
  , minPasswordLength
  , minUsernameLength
  , password
  , prependWithScheme
  , toBase64String
  , toPair
  , username
  ) where

import Prelude

import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Type.Proxy (Proxy(..))

type Config = { password ∷ NonEmptyString, username ∷ NonEmptyString }

prependWithScheme ∷ NonEmptyString → NonEmptyString
prependWithScheme nes = NEString.nes (Proxy ∷ _ "Basic ") <> nes

toPair ∷ NonEmptyString → NonEmptyString → NonEmptyString
toPair nes1 nes2 = nes1 <> NEString.nes (Proxy ∷ _ ":") <> nes2

toBase64String ∷ ∀ m. MonadEffect m ⇒ NonEmptyString → m String
toBase64String nes = liftEffect do
  buffer ←
    Buffer.fromString
      (NEString.toString nes)
      UTF8 ∷ Effect Buffer

  Buffer.toString Base64 buffer

password ∷ String → String \/ NonEmptyString
password s = do
  nes ← note "password cannot be empty" $ NEString.fromString s

  case NEString.length nes of
    l
      | l < minPasswordLength →
          Left "password is too short"

      | l > maxPasswordLength →
          Left "password is too long"

      | otherwise →
          Right nes

username ∷ String → String \/ NonEmptyString
username s = do
  nes ← note "username cannot be empty" $ NEString.fromString s

  case NEString.length nes of
    l
      | l < minUsernameLength →
          Left "username is too short"

      | l > maxUsernameLength →
          Left "username is too long"

      | otherwise →
          Right nes

maxPasswordLength ∷ Int
maxPasswordLength = 16

minPasswordLength ∷ Int
minPasswordLength = 1

maxUsernameLength ∷ Int
maxUsernameLength = 16

minUsernameLength ∷ Int
minUsernameLength = 1
