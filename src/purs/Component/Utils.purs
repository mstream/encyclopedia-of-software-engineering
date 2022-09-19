module Component.Utils
  ( OpaqueSlot
  , RawHTML(..)
  , classes
  , maxLength
  , minLength
  , size
  , unsafeSetInnerHtml
  ) where

import Prelude

import Data.Const (Const)
import Data.Enum (class BoundedEnum)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Unfoldable (class Unfoldable1)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..), PropName(..), Slot)
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP
import Routing.Duplex as RD
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

