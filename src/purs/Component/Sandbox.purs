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
  , renderForm
  ) where

import Prelude

import Component.Utils (OpaqueSlot, button, classes, submit)
import Control.Monad.State (put)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless (FormState)
import Halogen (Component, ComponentHTML, HalogenM, Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Web.Event.Internal.Types (Event)

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

renderForm
  ∷ ∀ action config slots m
  . (Event → action)
  → (config → action)
  → FormState
  → Presets config
  → Array (ComponentHTML action slots m)
  → ComponentHTML action slots m
renderForm
  handleSubmit
  handleApplyPreset
  formState
  presets
  fieldElements =
  HH.form
    [ HE.onSubmit handleSubmit ]
    ( renderPresets handleApplyPreset presets
        `Array.cons` fieldElements
        `Array.snoc` submitPanel formState
    )

renderPresets
  ∷ ∀ action config slots m
  . (config → action)
  → Presets config
  → ComponentHTML action slots m
renderPresets handleApplyPreset =
  HH.div [ classes [ "flex", "flex-col", "mb-4" ] ]
    <<< fromNonEmpty \preset presets →
      if Array.null presets then [ HH.text "" ]
      else
        [ HH.label_ [ HH.text "Configuration Presets" ]
        , HH.div [ classes [ "flex", "flex-row", "flex-wrap" ] ]
            $ [ renderPreset handleApplyPreset preset ]
                <> (renderPreset handleApplyPreset <$> presets)
                <> [ HH.hr_ ]
        ]

renderPreset
  ∷ ∀ action config slots m
  . (config → action)
  → Preset config
  → ComponentHTML action slots m
renderPreset handleApplyPreset (presetName /\ config) =
  button { action: handleApplyPreset config, label: presetName }

component
  ∷ ∀ config m
  . MonadAff m
  ⇒ NonEmptyString
  → FormComponent config m
  → SimulationComponent config m
  → SandboxComponent m
component title formComponent simulationComponent = H.mkComponent
  { initialState: const Nothing
  , render: render title formComponent simulationComponent
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

render
  ∷ ∀ config m
  . MonadAff m
  ⇒ NonEmptyString
  → FormComponent config m
  → SimulationComponent config m
  → State config
  → ComponentView config m
render title formComponent simulationComponent state =
  HH.div
    [ classes [ "border" ] ]
    [ HH.text $ NEString.toString title
    , HH.div
        [ classes [ "border", "flex", "flex-col" ] ]
        [ formPanel formComponent $ isNothing state
        , resetPanel $ isJust state
        , maybe (HH.text "") (simulationPanel simulationComponent) state
        ]
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

