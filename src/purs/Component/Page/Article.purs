module Component.Page.Article (component) where

import Prelude

import Control.Monad.State (put)
import Data.Array as Array
import Data.Article (Overview, Article)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NEString
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Component.Utils (classes)

type ComponentMonad m a = forall o. HalogenM State Action () o m a

type State = Input

type Input = Article

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
render article =
  HH.div
    [ classes [ "flex", "flex-col" ] ]
    [ HH.fromPlainHTML $ renderArticle article ]

renderArticle :: Article -> PlainHTML
renderArticle { overview, tags, title } =
  HH.article
    [ classes [ "flex", "flex-col" ] ]
    [ HH.header_ [ HH.h1_ [ HH.text $ NEString.toString title ] ], renderOverview overview ]

renderOverview :: Overview -> PlainHTML
renderOverview overview =
  HH.div
    [ classes [ "flex", "flex-col" ]
    ]
    (Array.fromFoldable $ (\s -> HH.p_ [ HH.text s ]) <$> overview)

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  Initialize →
    pure unit

  Receive input →
    put input
