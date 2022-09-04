module Utils (classes) where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Foldable (foldMap)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Location (hostname, protocol)
import Web.HTML.Window (location)

classes ∷ ∀ i r. Array String → IProp (class ∷ String | r) i
classes = HP.classes <<< map ClassName

