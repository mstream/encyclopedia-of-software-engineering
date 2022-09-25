module Component.Sandbox.CesarCypher (component) where

import Prelude

import Component.Sandbox (SandboxComponent)
import Component.Sandbox as Sandbox
import Component.Sandbox.CesarCypher.Form as Form
import Component.Sandbox.CesarCypher.Simulation as Simulation
import Control.Monad.Error.Class (class MonadThrow)
import Data.CesarCypher (Config)
import Data.CesarCypher as CesarCypher
import Data.Either (hush)
import Data.Maybe (fromJust)
import Data.NonEmpty ((:|))
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

component :: forall m. MonadAff m => MonadThrow Error m => SandboxComponent m
component = Sandbox.component
  (Form.component presets)
  Simulation.component
  where
  presets = defaultPreset :| []
  defaultPreset = NEString.nes (Proxy :: _ "default") /\
    unsafePreset 3 "Hello World"

unsafePreset :: Int -> String -> Config
unsafePreset i s = unsafePartial $ fromJust $ hush $ ado
  key <- CesarCypher.key i
  message <- CesarCypher.message s
  in { key, message }
