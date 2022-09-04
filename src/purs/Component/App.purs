module Component.App (Output(..), component) where

import Prelude

import Control.Monad.State (put)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (classes)

type ComponentMonad m a = HalogenM State Action () Output m a

type State = Input

type Input = Unit

data Output = Unit

data Action
  = Initialize
  | Receive Input

component ∷ ∀ q m. MonadAff m ⇒ Component q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState ∷ Input → State
initialState = identity

render ∷ ∀ m. State → ComponentHTML Action () m
render state =
  HH.div
    [ classes [ "flex", "flex-col" ] ]
    []

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  Initialize →
    pure unit

  Receive input →
    put input
