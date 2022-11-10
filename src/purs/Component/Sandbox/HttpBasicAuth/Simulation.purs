module Component.Sandbox.HttpBasicAuth.Simulation (component) where

import Prelude

import Component.Mermaid as Mermaid
import Component.Sandbox (SimulationComponent)
import Component.Utils (OpaqueSlot, classes)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , try
  )
import Control.Monad.State (get, modify_)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Http.BasicAuth (Config)
import Data.Http.BasicAuth as BasicAuth
import Data.Maybe (Maybe(..), maybe)
import Data.Mermaid (DiagramDef(..))
import Data.Mermaid.FlowChart
  ( FlowChartDef(..)
  , Orientation(..)
  , Segment(..)
  )
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, message, throw)
import Halogen (ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type ComponentMonad m a = HalogenM State Action ChildSlots Output m a
type ComponentView m = ComponentHTML Action ChildSlots m

type ChildSlots = (diagram ∷ OpaqueSlot Unit)

type Input = Config

type Output = Void

type Query ∷ ∀ a. a → Type
type Query = Const Void

type State =
  { encodedUsernameAndPassword ∷ Maybe (Error \/ NonEmptyString)
  , input ∷ Input
  }

data Action
  = Initialize
  | Receive Input

component
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadThrow Error m
  ⇒ SimulationComponent Config m
component = H.mkComponent
  { initialState: \input →
      { encodedUsernameAndPassword: Nothing
      , input
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

render ∷ ∀ m. MonadAff m ⇒ MonadThrow Error m ⇒ State → ComponentView m
render state = case state.encodedUsernameAndPassword of
  Nothing →
    HH.text ""

  Just (Left error) →
    HH.text $ "Base64 encoding error: " <> message error

  Just (Right encodedUsernameAndPassword) →
    HH.div
      [ classes [ "flex", "flex-row", "justify-center" ] ]
      [ HH.slot_
          (Proxy ∷ _ "diagram")
          unit
          Mermaid.component
          ( diagramDef
              state.input.username
              state.input.password
              encodedUsernameAndPassword
          )
      ]

handleAction ∷ ∀ m. MonadEffect m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  Initialize → do
    { input } ← get

    encodedUsernameAndPassword ← liftEffect
      $ try
      $ encodeUsernameAndPassword input

    modify_ _
      { encodedUsernameAndPassword = Just encodedUsernameAndPassword }

  Receive input → do
    modify_ _ { input = input }
    handleAction Initialize

encodeUsernameAndPassword
  ∷ ∀ m. MonadEffect m ⇒ MonadError Error m ⇒ Input → m NonEmptyString
encodeUsernameAndPassword { password, username } = liftEffect do
  s ← BasicAuth.toBase64String $ BasicAuth.toPair username password
  maybe (throw "empty string") pure (NEString.fromString s)

diagramDef
  ∷ NonEmptyString → NonEmptyString → NonEmptyString → DiagramDef
diagramDef username password encodedUsernameAndPassword =
  FlowChart $ FlowChartDef TopToBottom segments
  where
  segments ∷ Array Segment
  segments =
    let
      pair = BasicAuth.toPair username password
      headerValue = BasicAuth.prependWithScheme
        encodedUsernameAndPassword
    in
      [ Line
          "classDef text fill:#9f9,stroke:#333,stroke-width:4px;"
      , Line
          "classDef transformation fill:#f9f,stroke:#333,stroke-width:4px;"
      , Line
          $ textNodeDef "username"
          $ NEString.toString username
      , Line
          $ textNodeDef "password"
          $ NEString.toString password
      , Line
          $ textNodeDef "pair"
          $ NEString.toString
          $ pair
      , Line
          $ textNodeDef "encoded_pair"
          $ NEString.toString encodedUsernameAndPassword
      , Line
          $ textNodeDef "header_value"
          $ NEString.toString headerValue
      , Line
          $ transformationNodeDef
              "pair_transformation"
              "convert to a pair"
      , Line
          $ transformationNodeDef
              "base64_transformation"
              "encode with Base64"
      , Line
          $ transformationNodeDef
              "scheme_name_transformation"
              "prepend with a scheme name"
      , Line "username --> pair_transformation"
      , Line "password --> pair_transformation"
      , Line "pair_transformation --> pair"
      , Line "pair --> base64_transformation"
      , Line "base64_transformation --> encoded_pair"
      , Line "encoded_pair --> scheme_name_transformation"
      , Line "scheme_name_transformation --> header_value"
      , Line "linkStyle default stroke:green"
      ]

  textNodeDef ∷ String → String → String
  textNodeDef id s = id
    <> "["
    <> s
    <> "]; class "
    <> id
    <>
      " text;"

  transformationNodeDef ∷ String → String → String
  transformationNodeDef id s = id
    <> "[\\"
    <> s
    <> "/]; class "
    <> id
    <> " transformation"
