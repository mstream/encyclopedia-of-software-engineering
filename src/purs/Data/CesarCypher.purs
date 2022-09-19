module Data.CesarCypher
  ( Character(..)
  , Config(..)
  , Key(..)
  , Message(..)
  , decrypt
  , encrypt
  , key
  , maxKey
  , maxMessageLength
  , message
  , minKey
  , minMessageLength
  , toInt
  , toString
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Char as Char
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.NonEmpty as NEString
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Data.Traversable (traverse)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen as Gen

type Config = { key :: Key, message :: Message }

maxMessageLength :: Int
maxMessageLength = 16

minMessageLength :: Int
minMessageLength = 1

maxKey :: Int
maxKey = numberOfLetters - 1

minKey :: Int
minKey = 1

numberOfLetters :: Int
numberOfLetters = 26

charCodeOffset :: Int
charCodeOffset = 65

newtype Message = Message (NonEmptyArray Character)

message :: String -> String \/ Message
message s = do
  nes <- note "message cannot be empty"
    $ NEString.fromString
    $ String.toUpper s

  nea <- validateLength
    $ toNonEmptyCharArray nes

  Message <$> traverse
    ( \c -> case Char.toCharCode c of
        n | n == 32 ->
          Right Space

        n | n < charCodeOffset ->
          Left onlyAlphabeticMsg

        n | n >= charCodeOffset + numberOfLetters ->
          Left onlyAlphabeticMsg

        n ->
          Right $ Letter $ n - charCodeOffset
    )
    nea

  where
  onlyAlphabeticMsg = "only alphabetic characters and spaces allowed"

  validateLength nea = case NEArray.length nea of
    n | n < minMessageLength ->
      Left $ show n <> " character long message is too short"

    n | n > maxMessageLength ->
      Left $ show n <> " character long message is too long"

    _ ->
      Right nea

toString :: Message -> String
toString (Message characters) =
  String.fromCodePointArray
    $ Array.fromFoldable
    $ characterToCodePoint <$> characters
  where
  characterToCodePoint = case _ of
    Space ->
      String.codePointFromChar ' '

    Letter n ->
      String.codePointFromChar
        $ fromMaybe ' ' (Char.fromCharCode $ n + charCodeOffset)

newtype Key = Key Int

instance Arbitrary Key where
  arbitrary = Key <$> Gen.chooseInt minKey maxKey

key :: Int -> String \/ Key
key = case _ of
  i | i < minKey ->
    Left $ show i <> " is to small for a key"

  i | i > maxKey ->
    Left $ show i <> " is to large for a key"

  i ->
    Right $ Key i

toInt :: Key -> Int
toInt (Key i) = i

data Character = Letter Int | Space

derive instance Generic Character _
derive instance Eq Character

instance Show Character where
  show = genericShow

instance Arbitrary Character where
  arbitrary = do
    i <- Gen.chooseInt 0 9
    if i == 0 then pure Space else Letter <$> Gen.chooseInt 0 (numberOfLetters - 1)

encrypt :: Key -> Character -> Character
encrypt (Key k) = case _ of
  Space ->
    Space

  Letter n ->
    Letter $ (n + k) `mod` numberOfLetters

decrypt :: Key -> Character -> Character
decrypt (Key k) = case _ of
  Space ->
    Space

  Letter n ->
    Letter $ (n - k) `mod` numberOfLetters

