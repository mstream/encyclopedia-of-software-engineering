module Component.Sandbox.CesarCypher.Form (Output(..), Query, component) where

import Prelude

import Component.Sandbox (MakeFormComponent, Presets)
import Component.Sandbox as Sandbox
import Component.Utils (rangeInput, textInput)
import Control.Monad.State (get, put)
import Data.CesarCypher (Config, Key, Message)
import Data.CesarCypher as CesarCypher
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (head)
import Data.String.NonEmpty as NEString
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
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
import Type.Proxy (Proxy(..))

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
  ( message ∷ f String String Message
  , key ∷ f String String Key
  )

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

type State = Form

data Action
  = ApplyPreset Config
  | Eval (FormlessAction (FormFields FieldState))
  | Receive Form

component
  ∷ ∀ m
  . MonadAff m
  ⇒ MakeFormComponent Config m
component presets =
  Formless.formless
    { liftAction: Eval }
    (toInputs $ snd $ head presets)
    $ H.mkComponent
        { initialState: identity
        , render: render presets
        , eval: H.mkEval $ H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , receive = Just <<< Receive
            }
        }

render ∷ ∀ m. Presets Config → State → ComponentView m
render presets { actions, fields, formActions, formState } =
  Sandbox.renderForm
    formActions.handleSubmit
    ApplyPreset
    formState
    presets
    [ textInput
        { action: actions.message
        , label: NEString.nes (Proxy ∷ _ "message")
        , lengthRange: CesarCypher.minMessageLength
            /\ CesarCypher.maxMessageLength
        , placeholder: "foo"
        , state: fields.message
        }
    , rangeInput
        { action: actions.key
        , label: NEString.nes (Proxy ∷ _ "key")
        , state: fields.key
        , valueRange: CesarCypher.minKey /\ CesarCypher.maxKey

        }
    ]

handleAction ∷ ∀ m. MonadEffect m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  ApplyPreset config → do
    { formActions } ← get
    handleAction
      $ formActions.setFields
      $ Formless.mkFieldStates
      $ toInputs config

  Eval formAction →
    Formless.eval formAction

  Receive form →
    put form

handleQuery
  ∷ ∀ a m
  . FormQuery Query FormInputs FormResults FormOutputs a
  → ComponentMonad m (Maybe a)
handleQuery = do
  Formless.handleSubmitValidate
    Formless.raise
    Formless.validate
    { key: maybe (Left "key is not an integer") CesarCypher.key
        <<< Int.fromString
    , message: CesarCypher.message
    }

toInputs ∷ Config → Record FormInputs
toInputs { key, message } =
  { key: show $ CesarCypher.toInt key
  , message: CesarCypher.toString message
  }
