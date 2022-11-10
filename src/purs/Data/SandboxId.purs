module Data.SandboxId (SandboxId(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data SandboxId
  = BubbleSort
  | CesarCypher
  | HashCollision
  | HttpBasicAuth

derive instance Generic SandboxId _

derive instance Eq SandboxId

derive instance Ord SandboxId

instance Show SandboxId where
  show = genericShow

