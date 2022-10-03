module Component.Sandbox.BubbleSort.Form
  ( Output(..)
  , Query
  , component
  ) where

import Prelude

import Component.Sandbox (MakeFormComponent, Presets)
import Component.Sandbox as Sandbox
import Component.Utils (classes)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (get, gets, modify_)
import Data.Array ((!!))
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (head)
import Data.Sorting (Config)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Formless
  ( FieldAction
  , FieldInput
  , FieldOutput
  , FieldResult
  , FieldState
  , FormContext
  , FormOutput
  , FormQuery
  , FormlessAction
  )
import Formless as Formless
import Halogen (ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type ComponentMonad m a = HalogenM
  State
  Action
  ChildSlots
  (FormOutput (FormFields FieldState) Output)
  m
  a

type ComponentView m = ComponentHTML Action ChildSlots m

type ChildSlots ∷ ∀ slots. Row slots
type ChildSlots = ()

type FormFields ∷ (Type → Type → Type → Type) → Row Type
type FormFields f =
  (input ∷ f (Array Int) Void (Array Int))

type FormInputs = FormFields FieldInput
type FormOutputs = FormFields FieldOutput
type FormResults = FormFields FieldResult

type Form = FormContext
  (FormFields FieldState)
  (FormFields (FieldAction Action))
  Input
  Action

type Input = Unit
type Output = Config

type Query ∷ ∀ a. a → Type
type Query = Const Void

type State =
  { form ∷ Form
  , indices ∷ Indices
  }

type Indices = { dropTarget ∷ Maybe Int, liftedElement ∷ Maybe Int }

data Action
  = ApplyPreset Config
  | DropElement
  | Eval (FormlessAction (FormFields FieldState))
  | LiftElement Int
  | MakeDropTarget Int
  | Receive Form

component
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadThrow Error m
  ⇒ MakeFormComponent Config m
component presets =
  Formless.formless
    { liftAction: Eval }
    (toInputs $ snd $ head presets)
    $ H.mkComponent
        { initialState: \form →
            { form
            , indices: { dropTarget: Nothing, liftedElement: Nothing }
            }
        , render: render presets
        , eval: H.mkEval $ H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , receive = Just <<< Receive
            }
        }

render ∷ ∀ m. Presets Config → State → ComponentView m
render
  presets
  { form: { fields, formActions, formState }, indices } =
  Sandbox.renderForm
    formActions.handleSubmit
    ApplyPreset
    formState
    presets
    [ HH.div
        [ classes [ "flex", "flex-row" ] ]
        (renderElement indices `mapWithIndex` fields.input.value)
    ]

renderElement ∷ ∀ m. Indices → Int → Int → ComponentView m
renderElement indices idx value =
  HH.div
    [ HE.onDragStart $ const $ LiftElement idx
    , HE.onDragOver $ const $ MakeDropTarget idx
    , HE.onDragEnd $ const DropElement
    , HP.draggable true
    , classes
        [ "hover:bg-lime-800"
        , "align-middle"
        , if indices.dropTarget == Just idx then "bg-lime-800"
          else "bg-lime-900"
        , "cursor-move"
        , "flex"
        , "flex-row"
        , "h-12"
        , "justify-center"
        , "items-center"
        , "m-1"
        , "text-3xl"
        , "text-center"
        , "w-12"
        ]
    ]
    [ HH.text $ show value ]

handleAction ∷ ∀ m. MonadEffect m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  ApplyPreset config → do
    { formActions } ← gets _.form

    modify_ _
      { indices = { dropTarget: Nothing, liftedElement: Nothing } }

    handleAction
      $ formActions.setFields
      $ Formless.mkFieldStates
      $ toInputs config

  DropElement → do
    { indices, form: { fields } } ← get
    let
      newInput = fromMaybe fields.input.value
        case indices.dropTarget, indices.liftedElement of
          Just dropTarget, Just liftedElement → do
            liftedElementValue ← fields.input.value !! liftedElement
            dropTargetValue ← fields.input.value !! dropTarget
            pure
              $ Array.updateAtIndices
                  [ dropTarget /\ liftedElementValue
                  , liftedElement /\ dropTargetValue
                  ]
                  fields.input.value

          _, _ →
            Nothing

    handleAction $ ApplyPreset newInput

  Eval formAction →
    Formless.eval formAction

  LiftElement idx →
    modify_ \st →
      st { indices = st.indices { liftedElement = Just idx } }

  MakeDropTarget idx →
    modify_ \st →
      st { indices = st.indices { dropTarget = Just idx } }

  Receive form →
    modify_ _ { form = form }

handleQuery
  ∷ ∀ a m
  . FormQuery Query FormInputs FormResults FormOutputs a
  → ComponentMonad m (Maybe a)
handleQuery = do
  Formless.handleSubmitValidate
    (Formless.raise <<< _.input)
    Formless.validate
    { input: Right }

toInputs ∷ Config → Record FormInputs
toInputs elements = { input: elements }
