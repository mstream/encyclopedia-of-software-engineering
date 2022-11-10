module Component.Sandbox.HttpBasicAuth (component) where

import Component.Sandbox (SandboxComponent)
import Component.Sandbox as Sandbox
import Component.Sandbox.HttpBasicAuth.Form as Form
import Component.Sandbox.HttpBasicAuth.Simulation as Simulation
import Control.Monad.Error.Class (class MonadThrow)
import Data.NonEmpty ((:|))
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Type.Proxy (Proxy(..))

component
  ∷ ∀ m. MonadAff m ⇒ MonadThrow Error m ⇒ SandboxComponent m
component =
  Sandbox.component
    (NEString.nes (Proxy ∷ _ "Basic Authentication Header"))
    (Form.component presets)
    Simulation.component
  where
  presets = adminPreset :|
    []

  adminPreset = NEString.nes (Proxy ∷ _ "admin") /\
    { password: NEString.nes (Proxy ∷ _ "pass123")
    , username: NEString.nes (Proxy ∷ _ "admin")
    }

