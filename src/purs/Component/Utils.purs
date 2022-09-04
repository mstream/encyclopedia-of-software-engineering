module Component.Utils (OpaqueSlot, classes) where

import Prelude

import Halogen (ClassName(..), Slot)
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP

type OpaqueSlot slot = forall q. Slot q Void slot

classes ∷ ∀ i r. Array String → IProp (class ∷ String | r) i
classes = HP.classes <<< map ClassName

