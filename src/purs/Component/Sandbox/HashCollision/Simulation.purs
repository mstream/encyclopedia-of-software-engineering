module Component.Sandbox.HashCollision.Simulation (component) where

import Prelude

import Component.Utils (OpaqueSlot, classes)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (put)
import Data.Array as Array
import Data.Binary (Bit(..))
import Data.Binary as Binary
import Data.Const (Const)
import Data.Foldable (foldl)
import Data.HashCollision (Config)
import Data.HashCollision as HashCollision
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ComponentMonad m a = HalogenM State Action ChildSlots Output m a
type ComponentView m = ComponentHTML Action ChildSlots m

type ChildSlots = (diagram ∷ OpaqueSlot Unit)

type Input = Config

type Output = Void

type Query ∷ ∀ a. a → Type
type Query = Const Void

type State = Input

data Action = Receive Input

component
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadThrow Error m
  ⇒ Component Query Input Output m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

render ∷ ∀ m. MonadAff m ⇒ MonadThrow Error m ⇒ Config → ComponentView m
render { algorithm, input1, input2 } =
  HH.fromPlainHTML $ HH.table
    [ classes [ "border-collapse", "border-spacing-2" ] ]
    [ HH.tr_
        [ HH.th_
            [ HH.text $ hashFormula input1 ]
        , HH.td_
            [ HH.fromPlainHTML $ renderDigest inputDigestWithCollisions1
            ]
        ]
    , HH.tr_
        [ HH.th_
            [ HH.text $ hashFormula input2 ]
        , HH.td_
            [ HH.fromPlainHTML $ renderDigest inputDigestWithCollisions2
            ]
        ]
    , HH.tr_
        [ HH.th_ [ HH.text "collision ratio" ]
        , HH.td_ [ renderCollisionRatio collisionRatio ]
        ]
    ]
  where
  collisionRatio = (Int.toNumber $ 100 * numberOfCollidingBits) /
    (Int.toNumber totalNumberOfBits)
  hashFormula input = "hash(\"" <> input <> "\")"
  numberOfCollidingBits = foldl
    (\acc bits → acc + (List.length $ List.filter (eq Zero) bits))
    0
    collisionMask
  totalNumberOfBits = foldl
    (\acc bits → acc + List.length bits)
    0
    inputDigestBits1
  inputDigestWithCollisions1 = List.zipWith
    (\digestBits collisionBits → List.zip digestBits collisionBits)
    inputDigestBits1
    collisionMask
  inputDigestWithCollisions2 = List.zipWith
    (\digestBits collisionBits → List.zip digestBits collisionBits)
    inputDigestBits2
    collisionMask
  collisionMask = List.zipWith
    (\bits1 bits2 → List.zipWith Binary.xor bits1 bits2)
    inputDigestBits1
    inputDigestBits2
  inputDigestBits1 = Binary.toBits <$> HashCollision.digest algorithm
    input1
  inputDigestBits2 = Binary.toBits <$> HashCollision.digest algorithm
    input2

renderCollisionRatio ∷ Number → PlainHTML
renderCollisionRatio ratio =
  HH.span
    [ HP.style style, classes [ "p-1", "text-lg" ] ]
    [ HH.text $ toStringWith (fixed 1) ratio <> "%" ]
  where
  style = "color: rgb(" <> red <> "%," <> green <> "%,0%);"
  red = show $ Int.round ratio
  green = show $ Int.round $ 100.0 - ratio

renderDigest ∷ List (List (Bit /\ Bit)) → PlainHTML
renderDigest =
  HH.div
    [ classes
        [ "flex"
        , "flex-row"
        , "flex-wrap"
        , "font-mono"
        , "p-1"
        , "text-sm"
        ]
    ]
    <<< Array.fromFoldable
    <<< map renderByte

renderByte ∷ List (Bit /\ Bit) → PlainHTML
renderByte =
  HH.div [ classes [ "flex", "flex-nowrap", "flex-row", "mx-1" ] ]
    <<< Array.fromFoldable
    <<< map renderBit

renderBit ∷ Bit /\ Bit → PlainHTML
renderBit (digestBit /\ collisionBit) =
  HH.span
    [ classes
        [ if collisionBit == Zero then "text-red-500"
          else "text-green-500"
        ]
    ]
    [ HH.text $ case digestBit of
        Zero →
          "0"

        One →
          "1"
    ]

handleAction ∷ ∀ m. MonadEffect m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  Receive input →
    put input

