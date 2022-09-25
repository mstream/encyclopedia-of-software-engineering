module Component.Mermaid (component) where

import Prelude

import Data.Mermaid (DiagramDef)
import Data.Mermaid as Mermaid
import Component.RawHTML as RawHTML
import Component.Utils (RawHTML(..), OpaqueSlot)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (get, modify_)
import Control.Promise (Promise, toAffE)
import Data.Const (Const)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type ComponentMonad m a = HalogenM State Action ChildSlots Void m a
type ComponentView m = ComponentHTML Action ChildSlots m

type Input = DiagramDef

type SvgCode = RawHTML

type State = { diagramDef :: DiagramDef, svgCode :: Maybe SvgCode }

data Action = Initialize | Receive Input

type ChildSlots = (svg :: OpaqueSlot Unit)

type Query :: forall k. k -> Type
type Query = Const Void

component :: forall m. MonadAff m => MonadThrow Error m => Component Query Input Void m
component =
  H.mkComponent
    { initialState: \diagramDef -> { diagramDef, svgCode: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

handleAction :: forall m. MonadAff m => Action -> ComponentMonad m Unit
handleAction = case _ of
  Initialize -> do
    { diagramDef } <- get
    svgCode <- generateDiagram diagramDef
    modify_ _ { svgCode = Just svgCode }

  Receive diagramDef -> do
    modify_ _ { diagramDef = diagramDef }
    handleAction Initialize

render :: forall m. MonadAff m => MonadThrow Error m => State -> ComponentView m
render state = case state.svgCode of
  Nothing ->
    HH.text ""

  Just svgCode ->
    HH.slot_ (Proxy :: _ "svg") unit RawHTML.component svgCode

generateDiagram :: forall m. MonadAff m => DiagramDef -> m SvgCode
generateDiagram = map RawHTML
  <<< renderDiagramSvgCode "svgId"
  <<< Mermaid.toString

renderDiagramSvgCode :: forall m. MonadAff m => String -> String -> m String
renderDiagramSvgCode elementId diagramDef = liftAff
  $ toAffE
  $ runFn2 renderDiagramSvgCodeImpl elementId diagramDef

foreign import renderDiagramSvgCodeImpl :: Fn2 String String (Effect (Promise String))

