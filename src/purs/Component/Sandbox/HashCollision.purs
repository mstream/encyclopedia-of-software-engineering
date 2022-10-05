module Component.Sandbox.HashCollision (component) where

import Component.Sandbox (SandboxComponent)
import Component.Sandbox as Sandbox
import Component.Sandbox.HashCollision.Form as Form
import Component.Sandbox.HashCollision.Simulation as Simulation
import Data.HashCollision (Algorithm(..))
import Data.NonEmpty ((:|))
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Type.Proxy (Proxy(..))

component
  ∷ ∀ m. MonadAff m ⇒ SandboxComponent m
component =
  Sandbox.component
    (NEString.nes (Proxy ∷ _ "Hash Bit Collisions"))
    (Form.component presets)
    Simulation.component
  where
  presets = differentInputsPreset :| [ sameInputsPreset ]

  differentInputsPreset = NEString.nes (Proxy ∷ _ "different inputs") /\
    { algorithm: Md5, input1: "foo", input2: "bar" }

  sameInputsPreset = NEString.nes (Proxy ∷ _ "same inputs") /\
    { algorithm: Md5, input1: "foo", input2: "foo" }
