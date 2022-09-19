module Component.RawHTML where

import Prelude

import Component.Utils (RawHTML, unsafeSetInnerHtml)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State (get, put)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error)
import Halogen (ComponentHTML, HalogenM, RefLabel(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ComponentMonad m a = HalogenM State Action ChildSlots Void m a
type ComponentView m = ComponentHTML Action ChildSlots m

type State = Input

type Input = RawHTML

type ChildSlots :: forall k. Row k
type ChildSlots = ()

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | Receive Input

component :: forall m. MonadAff m => MonadThrow Error m => H.Component Query Input Void m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }

handleAction :: forall m. MonadEffect m => MonadThrow Error m => Action -> ComponentMonad m Unit
handleAction = case _ of
  Initialize -> do
    mbContainer <- H.getHTMLElementRef containerRefLabel

    case mbContainer of
      Nothing ->
        throwError $ error "container element not found"

      Just container -> do
        html <- get
        unsafeSetInnerHtml container html

  Receive html -> do
    put html

render :: forall m. State -> ComponentView m
render = const $ HH.div [ HP.ref containerRefLabel ] []

containerRefLabel :: RefLabel
containerRefLabel = RefLabel "container"
