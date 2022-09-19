module Component.Sandbox (FormComponent, SandboxComponent, SimulationComponent, component) where

import Prelude

import Component.Utils (OpaqueSlot, classes)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (put)
import Data.Const (Const)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Halogen (Component, ComponentHTML, HalogenM, Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type SandboxComponent m =
  Component (Const Void) Unit Void m

type FormComponent config m =
  Component (Const Void) Unit config m

type SimulationComponent config m =
  Component (Const Void) config Void m

type ComponentMonad config m a =
  HalogenM (State config) (Action config) (ChildSlots config) Output m a

type ComponentView config m =
  ComponentHTML (Action config) (ChildSlots config) m

type ChildSlots config =
  ( form :: Slot (Const Void) config Unit
  , simulation :: OpaqueSlot Unit
  )

type Input = Unit

type Output = Void

type Query :: forall a. a -> Type
type Query = Const Void

type State config = Maybe config

data Action config
  = HandleForm config
  | ResetSimulation

component
  :: forall config m
   . MonadAff m
  => MonadThrow Error m
  => FormComponent config m
  -> SimulationComponent config m
  -> SandboxComponent m
component formComponent simulationComponent = H.mkComponent
  { initialState: const Nothing
  , render: render formComponent simulationComponent
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

render
  :: forall config m
   . MonadAff m
  => MonadThrow Error m
  => FormComponent config m
  -> SimulationComponent config m
  -> State config
  -> ComponentView config m
render formComponent simulationComponent state =
  HH.div
    [ classes [ "border", "flex", "flex-col" ] ]
    [ HH.div
        [ classes [ if isJust state then "hidden" else "block" ] ]
        [ HH.slot (Proxy :: _ "form") unit formComponent unit HandleForm ]
    , HH.div
        [ classes [ if isJust state then "block" else "hidden" ] ]
        [ HH.button
            [ HE.onClick $ const ResetSimulation ]
            [ HH.text "Reset" ]
        ]
    , case state of
        Nothing ->
          HH.text ""

        Just config ->
          HH.div
            [ classes [ "flex", "flex-col" ] ]
            [ HH.hr_
            , HH.slot_
                (Proxy :: _ "simulation")
                unit
                simulationComponent
                config
            ]

    ]

handleAction :: forall config m. MonadEffect m => Action config -> ComponentMonad config m Unit
handleAction = case _ of
  HandleForm config ->
    put $ Just config

  ResetSimulation ->
    put Nothing

