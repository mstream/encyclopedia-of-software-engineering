module Component.Router where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Component.Page.Article as Article
import Component.Page.Article as Article
import Component.Page.Home as Home
import Component.Utils (OpaqueSlot)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Route (Route(..))
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Store (Store)
import Store as Store
import Type.Proxy (Proxy(..))

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
  => H.Component Query Unit Void m
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
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse Route.codec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute

    Receive _ ->
      pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      when (route /= Just dest) (H.modify_ _ { route = Just dest })
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home ->
        HH.slot_ (Proxy :: _ "home") unit Home.component unit
      Article article ->
        HH.slot_ (Proxy :: _ "article") unit Article.component article
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]

