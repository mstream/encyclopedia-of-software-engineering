module Component.Sandbox.CesarCypher (component) where

import Component.Sandbox (SandboxComponent)
import Component.Sandbox.CesarCypher.Form as Form
import Component.Sandbox.CesarCypher.Simulation as Simulation
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Component.Sandbox as Sandbox

component :: forall m. MonadAff m => MonadThrow Error m => SandboxComponent m
component = Sandbox.component Form.component Simulation.component
