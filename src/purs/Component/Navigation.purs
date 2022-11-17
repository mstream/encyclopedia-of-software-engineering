module Component.Navigation (component) where

import Prelude

import Component.Page.Article as Article
import Component.Utils (classes)
import Control.Monad.State (put)
import Data.Array as Array
import Data.ArticleId (ArticleId)
import Data.ArticleId as ArticleId
import Data.ArticleIndex as ArticleIndex
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum, upFromIncluding)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (SemigroupMap)
import Data.Paragraph (Segment(..))
import Data.Set (Set)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tag as Tag
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Utils (capitalize)

type ComponentMonad m a = ∀ o. HalogenM State Action ChildSlots o m a
type ComponentView m = ComponentHTML Action ChildSlots m

type ChildSlots ∷ ∀ k. Row k
type ChildSlots = ()

type State = View

type Input = Unit

data View
  = Alphabetical
  | Topical

derive instance Generic View _

derive instance Eq View

derive instance Ord View

instance Enum View where
  pred = genericPred
  succ = genericSucc

instance Bounded View where
  bottom = genericBottom
  top = genericTop

viewLabel ∷ View → NonEmptyString
viewLabel = case _ of
  Alphabetical →
    NEString.nes (Proxy ∷ _ "A-Z")

  Topical →
    NEString.nes (Proxy ∷ _ "By Topic")

data Action = ChangeView View

component
  ∷ ∀ m o q. Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

initialState ∷ Input → State
initialState = const Alphabetical

render ∷ ∀ m. State → ComponentView m
render currentView = HH.div
  [ classes [ "flex", "flex-col" ] ]
  [ renderTabs currentView
  , HH.fromPlainHTML $ case currentView of
      Alphabetical →
        renderAlphabetical

      Topical →
        renderTopical
  ]

renderTabs ∷ ∀ m. View → ComponentView m
renderTabs currentView = HH.div
  [ classes [ "flex", "flex-row-wrap", "justify-evenly" ] ]
  (renderTab <$> upFromIncluding bottom)
  where
  renderTab view = HH.div
    [ classes [ "p-1" ] ]
    [ HH.a
        [ HE.onClick $ const $ ChangeView view
        , classes
            [ if view == currentView then "font-semibold"
              else "font-normal"
            , "hover:cursor-pointer"
            , "hover:underline"
            , "cursor-default"
            , "no-underline"
            ]
        ]
        [ HH.text $ NEString.toString $ viewLabel view ]
    ]

renderAlphabetical ∷ PlainHTML
renderAlphabetical = renderBy
  String.singleton
  ArticleIndex.allByTitleFirstLetter

renderTopical ∷ PlainHTML
renderTopical = renderBy
  (capitalize <<< Tag.toString)
  ArticleIndex.allByTopic

renderBy
  ∷ ∀ a. (a → String) → SemigroupMap a (Set ArticleId) → PlainHTML
renderBy toString articleIdsByKey = HH.div
  [ classes [ "flex", "flex-col", "leading-7" ] ]
  (Array.fromFoldable $ renderEntry `mapWithIndex` articleIdsByKey)
  where
  renderEntry key articleIds = HH.div
    [ classes [ "flex", "flex-col" ] ]
    [ HH.div
        [ classes [ "bg-slate-800", "font-medium", "text-center" ] ]
        [ HH.text $ toString key ]
    , HH.div
        [ classes [ "flex", "flex-col", "pl-1" ] ]
        (renderItem <$> Array.fromFoldable articleIds)
    ]

  renderItem articleId = Article.renderSegment
    $ InternalReference
        (ArticleId.toNonEmptyString $ ArticleId.toTitle articleId)
        articleId

handleAction ∷ ∀ m. Action → ComponentMonad m Unit
handleAction = case _ of
  ChangeView view →
    put view

