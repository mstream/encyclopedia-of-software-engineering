module Data.SandboxIndex (ChildSlots, SandboxSlot, sandboxComponentById) where

import Prelude

import Component.Sandbox.BubbleSort as BubbleSort
import Component.Sandbox.CesarCypher as CesarCypher
import Component.Sandbox.HashCollision as HashCollision
import Component.Utils (OpaqueSlot)
import Control.Monad.Error.Class (class MonadThrow)
import Data.SandboxId (SandboxId(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Halogen (ComponentHTML)
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type ChildSlots =
  ( bubbleSort ∷ SandboxSlot
  , cesarCypher ∷ SandboxSlot
  , hashCollision ∷ SandboxSlot
  )

type SandboxSlot = OpaqueSlot Unit

sandboxComponentById
  ∷ ∀ action m
  . MonadAff m
  ⇒ MonadThrow Error m
  ⇒ SandboxId
  → ComponentHTML action ChildSlots m
sandboxComponentById = case _ of
  BubbleSort →
    HH.slot_
      (Proxy ∷ _ "bubbleSort")
      unit
      BubbleSort.component
      unit

  CesarCypher →
    HH.slot_
      (Proxy ∷ _ "cesarCypher")
      unit
      CesarCypher.component
      unit

  HashCollision →
    HH.slot_
      (Proxy ∷ _ "hashCollision")
      unit
      HashCollision.component
      unit

