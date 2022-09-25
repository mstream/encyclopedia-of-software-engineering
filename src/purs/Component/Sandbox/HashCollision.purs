module Component.Sandbox.HashCollision (component) where


import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Component.Sandbox (SandboxComponent)
import Component.Sandbox as Sandbox
import Component.Sandbox.HashCollision.Form as Form
import Component.Sandbox.HashCollision.Simulation as Simulation
import Control.Monad.Error.Class (class MonadThrow)
import Data.HashCollision (Algorithm(..))
import Data.String.NonEmpty as NEString
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Type.Proxy (Proxy(..))

component :: forall m. MonadAff m => MonadThrow Error m => SandboxComponent m
component =
  Sandbox.component
    (Form.component presets)
    Simulation.component
  where
  presets = defaultPreset :| [ sameInputsPreset ]

  defaultPreset = NEString.nes (Proxy :: _ "different inputs") /\
    { algorithm: Md5, input1: "foo", input2: "bar" }

  sameInputsPreset = NEString.nes (Proxy :: _ "same inputs") /\
    { algorithm: Md5, input1: "foo", input2: "foo" }
