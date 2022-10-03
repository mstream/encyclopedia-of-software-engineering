module Component.Utils
  ( OpaqueSlot
  , RawHTML(..)
  , RequestAnimationFrameId
  , animationFrameUpdateEmitter
  , button
  , classes
  , maxLength
  , minLength
  , radioGroup
  , rangeInput
  , requestAnimationFrame
  , size
  , submit
  , textInput
  , unsafeSetInnerHtml
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Formless (FieldAction, FieldState, FormState)
import Halogen (ClassName(..), ComponentHTML, PropName(..), Slot)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS
import Web.HTML (HTMLElement, Window, window)

newtype RawHTML = RawHTML String

type OpaqueSlot slot = Slot (Const Void) Void slot

maxLength ∷ ∀ i r. Int → IProp (maxLength ∷ Int | r) i
maxLength = HP.prop $ PropName "maxLength"

minLength ∷ ∀ i r. Int → IProp (minLength ∷ Int | r) i
minLength = HP.prop $ PropName "minLength"

size ∷ ∀ i r. Int → IProp (size ∷ Int | r) i
size = HP.prop $ PropName "size"

classes ∷ ∀ i r. Array String → IProp (class ∷ String | r) i
classes = HP.classes <<< map ClassName

unsafeSetInnerHtml ∷ ∀ m. MonadEffect m ⇒ HTMLElement → RawHTML → m Unit
unsafeSetInnerHtml element (RawHTML s) = liftEffect
  $ runFn2 unsafeSetInnerHtmlImpl element s

foreign import unsafeSetInnerHtmlImpl
  ∷ Fn2 HTMLElement String (Effect Unit)

type Button action = { action ∷ action, label ∷ NonEmptyString }

button ∷ ∀ action slots m. Button action → ComponentHTML action slots m
button { action, label } =
  HH.button
    [ HE.onClick $ const action
    , HP.type_ ButtonButton
    , classes
        [ "bg-sky-500"
        , "hover:bg-sky-400"
        , "m-1"
        , "p-1"
        , "rounded"
        ]
    ]
    [ HH.text $ NEString.toString label ]

type Submit = { formState ∷ FormState, label ∷ NonEmptyString }

submit ∷ ∀ action slots m. Submit → ComponentHTML action slots m
submit { formState, label } =
  HH.input
    [ HP.disabled $ formState.errorCount > 0
    , HP.type_ InputSubmit
    , HP.value $ NEString.toString label
    , classes
        [ "bg-sky-500"
        , "hover:bg-sky-400"
        , "m-1"
        , "p-1"
        , "rounded"
        ]
    ]

type RadioGroup action input output =
  { label ∷ NonEmptyString
  , state ∷ FieldState input Void output
  , action ∷ FieldAction action input Void output
  , options ∷
      Array
        { option ∷ input
        , render ∷ String
        , props ∷ Array (IProp HTMLinput action)
        }
  }

radioGroup
  ∷ ∀ input output action slots m
  . Eq input
  ⇒ RadioGroup action input output
  → ComponentHTML action slots m
radioGroup { label, state, action, options } =
  HH.div_
    [ HH.label_ [ HH.text $ NEString.toString label ]
    , HH.fieldset_ $ options <#> \{ option, render, props } →
        HH.label
          [ classes [ "m-1", "p-1" ] ]
          [ HH.input $ flip append props
              [ HP.type_ InputRadio
              , HP.name action.key
              , HP.checked $ state.value == option
              , HE.onChange $ const $ action.handleChange option
              , HE.onBlur action.handleBlur
              ]
          , HH.text render
          ]
    ]

type RangeInput action output =
  { action ∷ FieldAction action String String output
  , label ∷ NonEmptyString
  , state ∷ FieldState String String output
  , valueRange ∷ Int /\ Int
  }

rangeInput
  ∷ ∀ action output slots m
  . RangeInput action output
  → ComponentHTML action slots m
rangeInput { action, label, state, valueRange } =
  HH.div
    [ classes [ "flex", "flex-col", "mx-1", "my-2" ] ]
    [ HH.label_ [ HH.text $ NEString.toString label ]
    , HH.input
        [ HP.type_ InputRange
        , HP.min $ Int.toNumber $ fst valueRange
        , HP.max $ Int.toNumber $ snd valueRange
        , HP.value state.value
        , HE.onValueInput action.handleChange
        , HE.onBlur action.handleBlur
        , classes [ "bg-slate-800" ]
        ]
    , HH.text state.value
    ]

type TextInput action output =
  { action ∷ FieldAction action String String output
  , label ∷ NonEmptyString
  , lengthRange ∷ Int /\ Int
  , placeholder ∷ String
  , state ∷ FieldState String String output
  }

textInput
  ∷ ∀ action output slots m
  . TextInput action output
  → ComponentHTML action slots m
textInput { action, label, lengthRange, placeholder, state } =
  HH.div
    [ classes [ "flex", "flex-col", "mx-1", "my-2" ] ]
    [ HH.label_ [ HH.text $ NEString.toString label ]
    , HH.input
        [ HP.type_ InputText
        , HP.placeholder placeholder
        , size $ snd lengthRange
        , HP.value state.value
        , HE.onValueInput action.handleChange
        , HE.onBlur action.handleBlur
        , classes [ "bg-slate-800" ]
        , maxLength $ snd lengthRange
        , minLength $ fst lengthRange
        ]
    , HH.text case state.result of
        Just (Left errorMsg) →
          errorMsg

        _ →
          ""
    ]

animationFrameUpdateEmitter
  ∷ ∀ action m
  . MonadEffect m
  ⇒ (Number → action)
  → m (Emitter action)
animationFrameUpdateEmitter makeAction = liftEffect do
  { emitter, listener } ← HS.create
  win ← window

  let
    loop timestamp = do
      HS.notify listener $ makeAction timestamp
      void $ requestAnimationFrame win loop

  void $ requestAnimationFrame win loop

  pure emitter

newtype RequestAnimationFrameId = RequestAnimationFrameId Int

requestAnimationFrame
  ∷ Window → (Number → Effect Unit) → Effect RequestAnimationFrameId
requestAnimationFrame window cb = RequestAnimationFrameId
  <$> runFn2 requestAnimationFrameImpl window cb

foreign import requestAnimationFrameImpl
  ∷ Fn2 Window (Number → Effect Unit) (Effect Int)
