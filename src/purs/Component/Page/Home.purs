module Component.Page.Home (component) where

import Prelude

import Component.Utils (classes)
import Control.Monad.State (put)
import Data.Array as Array
import Data.Article (Overview, Article)
import Data.Article.Encryption (article)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NEString
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH

type ComponentMonad m a = forall o. HalogenM State Action () o m a

type State = Input

type Input = Unit

data Action
  = Initialize
  | Receive Input

component ∷ ∀ o q m. MonadAff m ⇒ Component q Input o m
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
    [ HH.text "Home" ]

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  Initialize →
    pure unit

  Receive input →
    put input
