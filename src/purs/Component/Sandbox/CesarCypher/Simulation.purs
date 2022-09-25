module Component.Sandbox.CesarCypher.Simulation (component) where

import Prelude

import Component.Mermaid as Mermaid
import Component.Sandbox (SimulationComponent)
import Component.Utils (OpaqueSlot)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (put)
import Data.Array as Array
import Data.CesarCypher (Character(..), Key(..), Message(..), Config)
import Data.CesarCypher as CesarCypher
import Data.Char as Char
import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Mermaid (DiagramDef(..))
import Data.Mermaid.FlowChart (FlowChartDef(..), Orientation(..), Segment(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Halogen (ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type ComponentMonad m a = HalogenM State Action ChildSlots Output m a
type ComponentView m = ComponentHTML Action ChildSlots m

type ChildSlots = (diagram :: OpaqueSlot Unit)

type Input = Config

type Output = Void

type Query :: forall a. a -> Type
type Query = Const Void

type State = Input

data Action = Receive Input

component
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => SimulationComponent Config m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

render :: forall m. MonadAff m => MonadThrow Error m => Config -> ComponentView m
render { key, message } =
  HH.slot_
    (Proxy :: _ "diagram")
    unit
    Mermaid.component
    (diagramDef message key)

handleAction :: forall m. MonadEffect m => Action -> ComponentMonad m Unit
handleAction = case _ of
  Receive input ->
    put input

diagramDef :: Message -> Key -> DiagramDef
diagramDef (Message nea) key =
  FlowChart $ FlowChartDef TopToBottom segments
  where
  segments :: Array Segment
  segments =
    [ SubGraph "input" [ Line $ String.joinWith "; " $ Array.fromFoldable $ inputNodeDef `mapWithIndex` nea ]
    , SubGraph "encoded" [ Line $ String.joinWith "; " $ Array.fromFoldable $ encryptedNodeDef key `mapWithIndex` nea ]
    , SubGraph "decoded" [ Line $ String.joinWith "; " $ Array.fromFoldable $ decryptedNodeDef `mapWithIndex` nea ]
    ] <> (Line <$> (Array.fromFoldable $ encryptionArrow key `mapWithIndex` nea))
      <> (Line <$> (Array.fromFoldable $ decryptionArrow key `mapWithIndex` nea))
      <> [ Line "linkStyle default stroke:green" ]

  inputNodeDef :: Int -> Character -> String
  inputNodeDef idx c = inputNodeId idx <> (nodeRep c)

  encryptedNodeDef :: Key -> Int -> Character -> String
  encryptedNodeDef k idx c = encryptedNodeId idx
    <> (nodeRep $ CesarCypher.encrypt k c)

  decryptedNodeDef :: Int -> Character -> String
  decryptedNodeDef idx c = decryptedNodeId idx <> nodeRep c

  encryptionArrow :: Key -> Int -> Character -> String
  encryptionArrow k idx c =
    inputNodeId idx
      <> " "
      <> arrow "<strong>»</strong>" k c
      <> " "
      <> encryptedNodeId idx

  decryptionArrow :: Key -> Int -> Character -> String
  decryptionArrow k idx c =
    encryptedNodeId idx
      <> " "
      <> arrow "<strong>«</strong>" k c
      <> " "
      <> decryptedNodeId idx

  arrow :: String -> Key -> Character -> String
  arrow symbol (Key d) = case _ of
    Space ->
      "-->"

    Letter _ ->
      "-->|\"" <> symbol <> show d <> symbol <> "\"|"

  inputNodeId :: Int -> String
  inputNodeId idx = "c" <> show idx

  encryptedNodeId :: Int -> String
  encryptedNodeId idx = inputNodeId idx <> "_encrypted"

  decryptedNodeId :: Int -> String
  decryptedNodeId idx = inputNodeId idx <> "_decrypted"

nodeRep :: Character -> String
nodeRep = case _ of
  Space ->
    "[\" \"]"

  Letter n ->
    "["
      <> maybe "ERROR" charToString (Char.fromCharCode $ n + 65)
      <> "]"
    where
    charToString c = "<p><strong>"
      <> String.singleton (String.codePointFromChar c)
      <> "</strong><sub>"
      <> show (n + 1)
      <> "</sub></p>"

