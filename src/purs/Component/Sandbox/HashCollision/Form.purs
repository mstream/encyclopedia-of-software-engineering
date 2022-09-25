module Component.Sandbox.HashCollision.Form
  ( Output(..)
  , Query
  , component
  ) where

import Prelude

import Component.Sandbox (MakeFormComponent, Presets)
import Component.Sandbox as Sandbox
import Component.Utils (radioGroup, textInput)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (get, put)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.HashCollision (Algorithm(..), Config)
import Data.HashCollision as HashCollision
import Data.Maybe (Maybe(..))
import Data.NonEmpty (head)
import Data.String.NonEmpty as NEString
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
  ( algorithm ∷ f Algorithm Void Algorithm
  , input1 ∷ f String String String
  , input2 ∷ f String String String
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
  ⇒ MonadThrow Error m
  ⇒ MakeFormComponent Config m
component presets =
  Formless.formless
    { liftAction: Eval }
    (snd $ head presets)
    $ H.mkComponent
        { initialState: identity
        , render: render presets
        , eval: H.mkEval $ H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , receive = Just <<< Receive
            }
        }

render ∷ ∀ m. Presets Config → Form → ComponentView m
render presets { actions, fields, formActions, formState } =
  Sandbox.renderForm
    formActions.handleSubmit
    ApplyPreset
    formState
    presets
    [ radioGroup
        { action: actions.algorithm
        , label: NEString.nes (Proxy ∷ _ "algorithm")
        , options:
            [ { option: Md5
              , render: HashCollision.toString Md5
              , props: []
              }
            , { option: Sha1
              , render: HashCollision.toString Sha1
              , props: []
              }
            , { option: Sha256
              , render: HashCollision.toString Sha256
              , props: []
              }
            , { option: Sha512
              , render: HashCollision.toString Sha512
              , props: []
              }
            ]
        , state: fields.algorithm
        }
    , textInput
        { action: actions.input1
        , label: NEString.nes (Proxy ∷ _ "input 1")
        , lengthRange: HashCollision.minInputLength
            /\ HashCollision.maxInputLength
        , placeholder: "foo"
        , state: fields.input1
        }
    , textInput
        { action: actions.input2
        , label: NEString.nes (Proxy ∷ _ "input 2")
        , lengthRange: HashCollision.minInputLength
            /\ HashCollision.maxInputLength
        , placeholder: "foo"
        , state: fields.input2
        }
    ]

handleAction ∷ ∀ m. MonadEffect m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  ApplyPreset config → do
    { formActions } ← get
    handleAction $ formActions.setFields $ Formless.mkFieldStates config

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
    { algorithm: Right
    , input1: HashCollision.input
    , input2: HashCollision.input
    }

