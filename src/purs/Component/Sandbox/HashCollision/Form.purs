module Component.Sandbox.HashCollision.Form
  ( Output(..)
  , Query
  , component
  ) where

import Prelude

import Component.Sandbox (MakeFormComponent, Preset, Presets)
import Component.Sandbox as SandboxComponent
import Component.Utils (button, classes, maxLength, radioGroup, size)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (get, put)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.HashCollision (Algorithm(..), Config)
import Data.HashCollision as HashCollision
import Data.Maybe (Maybe(..))
import Data.NonEmpty (fromNonEmpty, head)
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
import Halogen.HTML.Properties (InputType(..))
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
  HH.form
    [ HE.onSubmit formActions.handleSubmit ]
    [ renderPresets presets
    , radioGroup
        { action: actions.algorithm
        , label: "algorithm"
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
    , HH.div
        [ classes [ "flex", "flex-col", "mx-1", "my-2" ] ]
        [ HH.label_ [ HH.text "input 1" ]
        , HH.input
            [ HP.type_ InputText
            , HP.placeholder "message"
            , size HashCollision.maxInputLength
            , HP.value fields.input1.value
            , HE.onValueInput actions.input1.handleChange
            , HE.onBlur actions.input1.handleBlur
            , classes [ "bg-slate-800" ]
            , maxLength HashCollision.maxInputLength
            ]
        , HH.text case fields.input1.result of
            Just (Left errorMsg) →
              errorMsg

            _ →
              ""
        ]
    , HH.div
        [ classes [ "flex", "flex-col", "mx-1", "my-2" ] ]
        [ HH.label_ [ HH.text "input 2" ]
        , HH.input
            [ HP.type_ InputText
            , HP.placeholder "message"
            , size HashCollision.maxInputLength
            , HP.value fields.input2.value
            , HE.onValueInput actions.input2.handleChange
            , HE.onBlur actions.input2.handleBlur
            , classes [ "bg-slate-800" ]
            , maxLength HashCollision.maxInputLength
            ]
        , HH.text case fields.input2.result of
            Just (Left errorMsg) →
              errorMsg

            _ →
              ""
        ]
    , SandboxComponent.submitPanel formState
    ]

renderPresets
  ∷ ∀ m
  . Presets Config
  → ComponentView m
renderPresets = HH.div [ classes [ "flex", "flex-col", "mb-4" ] ]
  <<< fromNonEmpty \preset presets →
    if Array.null presets then [ HH.text "" ]
    else
      [ HH.label_ [ HH.text "Configuration Presets" ]
      , HH.div [ classes [ "flex", "flex-row", "flex-wrap" ] ]
          $ [ renderPreset preset ]
              <> (renderPreset <$> presets)
              <> [ HH.hr_ ]
      ]

renderPreset ∷ ∀ m. Preset Config → ComponentView m
renderPreset (presetName /\ config) =
  button { action: ApplyPreset config, label: presetName }

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
    { algorithm: validateAlgorithm
    , input1: validateInput
    , input2: validateInput
    }
  where
  validateAlgorithm = Right

  validateInput = HashCollision.input

