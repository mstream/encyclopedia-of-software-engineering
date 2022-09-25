module Component.Sandbox.CesarCypher.Form (Output(..), Query, component) where

import Prelude

import Component.Sandbox (MakeFormComponent)
import Component.Utils (classes, maxLength, minLength, size)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (put)
import Data.CesarCypher (Config, Key, Message)
import Data.CesarCypher as CesarCypher
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.NonEmpty (head)
import Data.Tuple (snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Formless (FieldAction, FieldInput, FieldOutput, FieldState, FormContext, FormOutput, FormQuery, FormlessAction, FieldResult)
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

type ChildSlots :: forall slots. Row slots
type ChildSlots = ()

type FormFields :: (Type -> Type -> Type -> Type) -> Row Type
type FormFields f =
  ( message :: f String String Message
  , key :: f String String Key
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

type Query :: forall a. a -> Type
type Query = Const Void

type State = Form

data Action
  = Eval (FormlessAction (FormFields FieldState))
  | Receive Form

component
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => MakeFormComponent Config m
component presets =
  Formless.formless
    { liftAction: Eval }
    (toInputs $ snd $ head presets)
    $ H.mkComponent
        { initialState: identity
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , receive = Just <<< Receive
            }
        }

render :: forall m. Form -> ComponentView m
render { actions, fields, formActions, formState } =
  HH.form
    [ HE.onSubmit formActions.handleSubmit ]
    [ HH.div
        [ classes [ "flex", "flex-col", "mx-1", "my-2" ] ]
        [ HH.label_ [ HH.text "message" ]
        , HH.input
            [ HP.type_ InputText
            , HP.placeholder "message"
            , size CesarCypher.maxMessageLength
            , HP.value fields.message.value
            , HE.onValueInput actions.message.handleChange
            , HE.onBlur actions.message.handleBlur
            , classes [ "bg-slate-800" ]
            , maxLength CesarCypher.maxMessageLength
            , minLength CesarCypher.minMessageLength
            ]
        , HH.text case fields.message.result of
            Just (Left errorMsg) ->
              errorMsg

            _ ->
              ""
        ]
    , HH.div
        [ classes [ "flex", "flex-col", "mx-1", "my-2" ] ]
        [ HH.label_ [ HH.text "key" ]
        , HH.input
            [ HP.type_ InputRange
            , HP.min $ Int.toNumber CesarCypher.minKey
            , HP.max $ Int.toNumber CesarCypher.maxKey
            , HP.placeholder "key"
            , HP.value fields.key.value
            , HE.onValueInput actions.key.handleChange
            , HE.onBlur actions.key.handleBlur
            , classes [ "bg-slate-800" ]
            ]
        , HH.text fields.key.value
        , HH.text case fields.key.result of
            Just (Left errorMsg) ->
              errorMsg

            _ ->
              ""
        ]
    , HH.input
        [ HP.type_ InputSubmit
        , HP.disabled $ formState.errorCount > 0
        , HP.value "Start"
        ]
    ]

handleAction :: forall m. MonadEffect m => Action -> ComponentMonad m Unit
handleAction = case _ of
  Eval formAction ->
    Formless.eval formAction

  Receive form ->
    put form

handleQuery
  :: forall a m
   . FormQuery Query FormInputs FormResults FormOutputs a
  -> ComponentMonad m (Maybe a)
handleQuery = do
  Formless.handleSubmitValidate
    Formless.raise
    Formless.validate
    { key: validateKey, message: validateMessage }
  where
  validateKey = Int.fromString >>> case _ of
    Nothing ->
      Left "key is not an integer"

    Just i ->
      CesarCypher.key i

  validateMessage = CesarCypher.message

toInputs :: Config -> Record FormInputs
toInputs { key, message } =
  { key: show $ CesarCypher.toInt key
  , message: CesarCypher.toString message
  }
