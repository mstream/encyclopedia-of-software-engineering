module Component.Utils
  ( OpaqueSlot
  , RawHTML(..)
  , button
  , classes
  , maxLength
  , minLength
  , radioGroup
  , size
  , submit
  , unsafeSetInnerHtml
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Const (Const)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Formless (FieldAction, FieldState, FormState)
import Halogen (ClassName(..), PropName(..), Slot, ComponentHTML)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

newtype RawHTML = RawHTML String

type OpaqueSlot slot = Slot (Const Void) Void slot

maxLength :: forall i r. Int -> IProp (maxLength :: Int | r) i
maxLength = HP.prop $ PropName "maxLength"

minLength :: forall i r. Int -> IProp (minLength :: Int | r) i
minLength = HP.prop $ PropName "minLength"

size :: forall i r. Int -> IProp (size :: Int | r) i
size = HP.prop $ PropName "size"

classes ∷ ∀ i r. Array String → IProp (class ∷ String | r) i
classes = HP.classes <<< map ClassName

unsafeSetInnerHtml :: forall m. MonadEffect m => HTMLElement -> RawHTML -> m Unit
unsafeSetInnerHtml element (RawHTML s) = liftEffect
  $ runFn2 unsafeSetInnerHtmlImpl element s

foreign import unsafeSetInnerHtmlImpl :: Fn2 HTMLElement String (Effect Unit)

type Button action = { action :: action, label :: NonEmptyString }

button :: forall action slots m. Button action -> ComponentHTML action slots m
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

type Submit = { formState :: FormState, label :: NonEmptyString }

submit :: forall action slots m. Submit -> ComponentHTML action slots m
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
  { label :: String
  , state :: FieldState input Void output
  , action :: FieldAction action input Void output
  , options ::
      Array
        { option :: input
        , render :: String
        , props :: Array (IProp HTMLinput action)
        }
  }

radioGroup
  :: forall input output action slots m
   . Eq input
  => RadioGroup action input output
  -> ComponentHTML action slots m
radioGroup { label, state, action, options } =
  HH.div_
    [ HH.label_ [ HH.text label ]
    , HH.fieldset_ $ options <#> \{ option, render, props } ->
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
