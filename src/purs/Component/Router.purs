module Component.Router (Query(..), component) where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Component.Page.Article as Article
import Component.Page.Home as Home
import Component.Utils (OpaqueSlot, classes)
import Data.ArticleId (ArticleId)
import Data.ArticleId as ArticleId
import Data.Either (hush)
import Data.Enum (upFromIncluding)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Paragraph (Segment(..))
import Data.Route (Route(..))
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Store (Store)
import Store as Store
import Type.Proxy (Proxy(..))

type ComponentMonad m a = HalogenM State Action ChildSlots Void m a
type ComponentView m = ComponentHTML Action ChildSlots m

data Query a = Navigate Route a

type State = { route :: Maybe Route }

data Action
  = Initialize
  | Receive (Connected Store Unit)

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , article :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => Component Query Unit Void m
component = connect (selectEq identity)
  $ H.mkComponent
      { initialState: const { route: Nothing }
      , render
      , eval: H.mkEval $ H.defaultEval
          { handleQuery = handleQuery
          , handleAction = handleAction
          , receive = Just <<< Receive
          , initialize = Just Initialize
          }
      }

handleAction :: forall m. MonadAff m => Navigate m => Action -> ComponentMonad m Unit
handleAction = case _ of
  Initialize -> do
    initialRoute <- hush <<< (RD.parse Route.codec) <$> liftEffect getHash
    navigate $ fromMaybe Home initialRoute

  Receive _ ->
    pure unit

handleQuery :: forall a m. MonadAff m => Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
handleQuery = case _ of
  Navigate dest a -> do
    { route } <- H.get
    when (route /= Just dest) (H.modify_ _ { route = Just dest })
    pure (Just a)

render :: forall m. MonadAff m => State -> ComponentView m
render { route } =
  HH.div
    [ classes
        [ "bg-slate-900"
        , "flex"
        , "flex-col"
        , "h-screen"
        , "justify-between"
        , "text-slate-50"
        , "w-screen"
        ]
    ]
    [ case route of
        Just Home ->
          renderFound Nothing

        Just (Article articleId) ->
          renderFound $ Just articleId

        Nothing ->
          HH.fromPlainHTML renderNotFound
    , HH.fromPlainHTML renderFooter
    ]

renderNotFound :: PlainHTML
renderNotFound =
  HH.div_ [ HH.text "Oh no! That page wasn't found." ]

renderFound :: forall m. MonadAff m => Maybe ArticleId -> ComponentView m
renderFound mbArticleId =
  HH.div
    [ classes [ "flex", "flex-row" ] ]
    [ HH.fromPlainHTML renderNavigation
    , HH.main
        [ classes [ "basis-10/12" ] ]
        [ case mbArticleId of
            Nothing ->
              HH.slot_
                (Proxy :: _ "home")
                unit
                Home.component
                unit

            Just articleId ->
              HH.slot_
                (Proxy :: _ "article")
                unit
                Article.component
                articleId
        ]
    ]

renderNavigation :: PlainHTML
renderNavigation = HH.aside
  [ classes [ "basis-2/12", "flex", "flex-col" ] ]
  (renderItem <$> upFromIncluding bottom)
  where
  renderItem articleId = Article.renderSegment
    $ InternalReference
        (ArticleId.toNonEmptyString $ ArticleId.toTitle articleId)
        articleId

renderFooter :: PlainHTML
renderFooter = HH.footer
  [ classes [ "leading-6", "mt-16", "text-center" ] ]
  [ HH.text "TODO(footer)" ]

