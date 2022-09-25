module Component.Sandbox
  ( FormComponent
  , Input
  , MakeFormComponent
  , Preset
  , Presets
  , Query
  , SandboxComponent
  , SimulationComponent
  , component
  , submitPanel
  ) where

import Prelude

import Component.Utils (OpaqueSlot, button, classes, submit)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (put)
import Data.Const (Const)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.NonEmpty (NonEmpty)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested (type (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Formless (FormState)
import Halogen (Component, ComponentHTML, HalogenM, Slot)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type SandboxComponent m =
  Component Query Input Void m

type MakeFormComponent config m =
  NonEmpty Array (Preset config) → FormComponent config m

type Presets config = NonEmpty Array (Preset config)

type Preset config = NonEmptyString /\ config

type FormComponent config m =
  Component (Const Void) Unit config m

type SimulationComponent config m =
  Component (Const Void) config Void m

type ComponentMonad config m a =
  HalogenM (State config) (Action config) (ChildSlots config) Output m a

type ComponentView config m =
  ComponentHTML (Action config) (ChildSlots config) m

type ChildSlots config =
  ( form ∷ Slot (Const Void) config Unit
  , simulation ∷ OpaqueSlot Unit
  )

type Input = Unit

type Output = Void

type Query ∷ ∀ a. a → Type
type Query = Const Void

type State config = Maybe config

data Action config
  = HandleForm config
  | ResetSimulation

component
  ∷ ∀ config m
  . MonadAff m
  ⇒ MonadThrow Error m
  ⇒ FormComponent config m
  → SimulationComponent config m
  → SandboxComponent m
component formComponent simulationComponent = H.mkComponent
  { initialState: const Nothing
  , render: render formComponent simulationComponent
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

render
  ∷ ∀ config m
  . MonadAff m
  ⇒ MonadThrow Error m
  ⇒ FormComponent config m
  → SimulationComponent config m
  → State config
  → ComponentView config m
render formComponent simulationComponent state =
  HH.div
    [ classes [ "border", "flex", "flex-col" ] ]
    [ formPanel formComponent $ isNothing state
    , resetPanel $ isJust state
    , maybe (HH.text "") (simulationPanel simulationComponent) state
    ]

handleAction
  ∷ ∀ config m
  . MonadEffect m
  ⇒ Action config
  → ComponentMonad config m Unit
handleAction = case _ of
  HandleForm config →
    put $ Just config

  ResetSimulation →
    put Nothing

submitPanel ∷ ∀ action slots m. FormState → ComponentHTML action slots m
submitPanel formState =
  HH.div
    [ classes [ "flex", "flex-row", "justify-center", "mt-4" ] ]
    [ submit { formState, label: NEString.nes (Proxy ∷ _ "Start") } ]

resetPanel ∷ ∀ config m. Boolean → ComponentView config m
resetPanel isVisible = HH.div
  [ classes
      [ if isVisible then "block" else "hidden"
      , "flex"
      , "flex-row"
      , "justify-center"
      ]
  ]
  [ button
      { action: ResetSimulation
      , label: NEString.nes (Proxy ∷ _ "Reset")
      }
  ]

formPanel
  ∷ ∀ config m
  . FormComponent config m
  → Boolean
  → ComponentView config m
formPanel formComponent isVisible = HH.div
  [ classes [ if isVisible then "block" else "hidden" ] ]
  [ HH.slot (Proxy ∷ _ "form") unit formComponent unit HandleForm
  ]

simulationPanel
  ∷ ∀ config m
  . SimulationComponent config m
  → config
  → ComponentView config m
simulationPanel simulationComponent config = HH.div
  [ classes [ "flex", "flex-col" ] ]
  [ HH.hr_
  , HH.slot_
      (Proxy ∷ _ "simulation")
      unit
      simulationComponent
      config
  ]
