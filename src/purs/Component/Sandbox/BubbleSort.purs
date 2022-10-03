module Component.Sandbox.BubbleSort (component) where

import Component.Sandbox (SandboxComponent)
import Component.Sandbox as Sandbox
import Component.Sandbox.BubbleSort.Form as Form
import Component.Sandbox.BubbleSort.Simulation as Simulation
import Control.Monad.Error.Class (class MonadThrow)
import Data.NonEmpty ((:|))
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Type.Proxy (Proxy(..))

component ∷ ∀ m. MonadAff m ⇒ MonadThrow Error m ⇒ SandboxComponent m
component =
  Sandbox.component
    (Form.component presets)
    Simulation.component
  where
  presets = unsortedRandomPreset :|
    [ unsortedDescendingPreset, sortedPreset ]

  unsortedRandomPreset = NEString.nes (Proxy ∷ _ "unsorted - random") /\
    [ 5, 2, 3, 7, 8, 1, 4, 6 ]

  unsortedDescendingPreset =
    NEString.nes (Proxy ∷ _ "unsorted - descending") /\
      [ 8, 7, 6, 5, 4, 3, 2, 1 ]

  sortedPreset = NEString.nes (Proxy ∷ _ "sorted") /\
    [ 1, 2, 3, 4, 5, 6, 7, 8 ]
