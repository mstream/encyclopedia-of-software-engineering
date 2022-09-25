module Data.Binary
  ( Bit(..)
  , Byte
  , fromList
  , toBits
  , xor
  ) where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

data Byte = Byte Bit Bit Bit Bit Bit Bit Bit Bit

toBits :: Byte -> List Bit
toBits (Byte b1 b2 b3 b4 b5 b6 b7 b8) =
  b1 : b2 : b3 : b4 : b5 : b6 : b7 : b8 : Nil

fromList :: List Bit -> Maybe Byte
fromList = case _ of
  b1 : b2 : b3 : b4 : b5 : b6 : b7 : b8 : Nil ->
    Just $ Byte b1 b2 b3 b4 b5 b6 b7 b8

  _ ->
    Nothing

xor :: Bit -> Bit -> Bit
xor bit1 bit2 = if bit1 == bit2 then Zero else One

data Bit = Zero | One

derive instance Eq Bit
