module Component.Sandbox.BubbleSort.Simulation (component) where

import Prelude

import CSS (AnimationName(..), CSS)
import CSS
  ( animation
  , border
  , easeInOut
  , forwards
  , fromString
  , iterationCount
  , keyframes
  , left
  , normalAnimationDirection
  , px
  , rem
  , sec
  , solid
  , top
  , white
  ) as CSS
import Component.Utils
  ( OpaqueSlot
  , animationFrameUpdateEmitter
  , classes
  )
import Control.Monad.State (get, modify_, put)
import Data.Array ((!!))
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Program (Event(..))
import Data.SlidingList (SlidingList)
import Data.SlidingList as SlidingList
import Data.Sorting (Config)
import Data.Sorting as Sorting
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style, stylesheet) as CSS

type ComponentMonad m a = HalogenM State Action ChildSlots Output m a
type ComponentView m = ComponentHTML Action ChildSlots m

type ChildSlots = (diagram ∷ OpaqueSlot Unit)

type Input = Config

type Output = Void

type Query ∷ ∀ a. a → Type
type Query = Const Void

type State =
  { animation ∷ String \/ Animation
  , animationTime ∷ Number
  }

type Animation = SlidingList Frame

type Frame =
  { elementsAfter ∷ Array Int
  , elementsBefore ∷ Array Int
  , event ∷ Event
  , eyeIndexAfter ∷ Int
  , eyeIndexBefore ∷ Int
  , eyeIndexLimit ∷ Int
  }

data Action = Initialize | Receive Input | UpdateAnimationFrame Number

data ShiftDirection = ShiftLeft | ShiftRight

shiftDirectionToString ∷ ShiftDirection → String
shiftDirectionToString = case _ of
  ShiftLeft →
    "shift-left"

  ShiftRight →
    "shift-right"

eyeMovementIndexToString ∷ Int → String
eyeMovementIndexToString idx =
  "move-eye-" <> show idx

component
  ∷ ∀ m
  . MonadAff m
  ⇒ Component Query Input Output m
component = H.mkComponent
  { initialState: \input →
      { animation: generateAnimation input
      , animationTime: 0.0
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

render ∷ ∀ m. MonadAff m ⇒ State → ComponentView m
render state = case currentFrame of
  Left errorMsg → HH.text errorMsg
  Right frame →
    let
      elementIndices = Array.range 0
        $ Array.length frame.elementsBefore - 1
    in
      HH.fromPlainHTML $ HH.div
        [ classes [ "flex", "flex-col" ] ]
        [ CSS.stylesheet do
            shiftKeyframes ShiftLeft
            shiftKeyframes ShiftRight
            traverse_ eyeMovementKeyframes elementIndices
        , HH.div
            [ classes [ "flex", "flex-row" ]
            ]
            ( renderElement frame.event frame.eyeIndexLimit
                `mapWithIndex` frame.elementsBefore
            )
        , renderEye frame
        , HH.div
            [ classes [ "flex", "flex-row" ] ]
            $ renderElementIndex <$> elementIndices

        , HH.text case frame.event of

            InputElementsCompared idx1 idx2 _ →
              "compared element at "
                <> show idx1
                <> " with element at "
                <> show idx2

            InputElementsSwapped idx1 idx2 →
              "swapped element at "
                <> show idx1
                <> " with element at "
                <> show idx2

            Printed s →
              "printed '" <> s <> "'"

            VariableSet name value →
              "variable " <> name <> " set to " <> show value
        ]
  where
  currentFrame = do
    animation ← state.animation
    note "animation error" $ SlidingList.currentItem animation

renderEye ∷ Frame → PlainHTML
renderEye frame = HH.div
  [ CSS.style do
      CSS.left $ CSS.rem $ eyeLeft frame.eyeIndexBefore
      CSS.top $ CSS.rem (-7.5)

      case frame.event of
        VariableSet name idx →
          if name == "j" then eyeMovementAnimation idx else pure unit

        _ →
          pure unit
  , classes
      [ "align-middle"
      , "aspect-square"
      , "font-medium"
      , "m-8"
      , "relative"
      , "text-4xl"
      , "text-center"
      , "w-16"
      ]
  ]
  [ case frame.event of
      InputElementsCompared _ _ LT →
        HH.span
          [ classes [ "text-green-500" ] ]
          [ HH.text "<" ]

      InputElementsCompared _ _ EQ →
        HH.span
          [ classes [ "text-green-500" ] ]
          [ HH.text "=" ]

      InputElementsCompared _ _ GT →
        HH.span
          [ classes [ "text-red-500" ] ]
          [ HH.text ">" ]

      _ →
        HH.span
          [ classes [ "text-gray-500" ] ]
          [ HH.text $ if frame.eyeIndexLimit == 0 then "" else "?" ]
  ]

renderElement ∷ Event → Int → Int → Int → PlainHTML
renderElement event eyeIndexLimit idx value =
  HH.div
    [ CSS.style case event of
        InputElementsCompared idx1 idx2 _ →
          if idx == idx1 || idx == idx2 then CSS.border
            CSS.solid
            (CSS.px 1.0)
            CSS.white
          else pure unit

        InputElementsSwapped idx1 idx2 → case idx of
          i
            | i == idx1 → shiftAnimation ShiftRight
            | i == idx2 → shiftAnimation ShiftLeft
            | otherwise → pure unit

        _ →
          pure unit
    , classes
        [ "flex"
        , "flex-row"
        , if idx > eyeIndexLimit || eyeIndexLimit == 0 then
            "bg-green-500"
          else "bg-lime-900"
        , "h-16"
        , "m-8"
        , "justify-center"
        , "items-center"
        , "relative"
        , "text-3xl"
        , "w-16"
        ]
    ]
    [ HH.text $ show value ]

renderElementIndex ∷ Int → PlainHTML
renderElementIndex idx = HH.div
  [ CSS.style do
      CSS.top $ CSS.rem (-12.0)
  , classes
      [ "align-middle"
      , "font-light"
      , "h-16"
      , "italics"
      , "m-8"
      , "relative"
      , "text-sm"
      , "text-center"
      , "w-16"
      ]
  ]
  [ HH.text $ show idx ]

handleAction ∷ ∀ m. MonadEffect m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  Initialize → do
    emitter ← animationFrameUpdateEmitter UpdateAnimationFrame
    void $ H.subscribe emitter

  Receive input →
    put $ { animation: generateAnimation input, animationTime: 0.0 }

  UpdateAnimationFrame timestamp → do
    { animationTime } ← get
    if timestamp - animationTime < 1500.0 then pure unit
    else modify_ \st → st
      { animation = st.animation <#> \animation →
          fromMaybe animation $ SlidingList.slideForwards animation
      , animationTime = timestamp
      }

generateAnimation ∷ Config → String \/ Animation
generateAnimation elements = do
  { events } ← case Sorting.bubble elements of
    Left _ →
      Left "program execution error"

    Right events →
      Right events

  frames ← foldM f Nil events
  pure $ SlidingList.fromFoldable $ List.reverse frames

  where
  f frames event =
    let
      elementsBefore = maybe
        elements
        (_.elementsAfter)
        (List.head frames)

      eyeIndexBefore = maybe
        0
        (_.eyeIndexAfter)
        (List.head frames)

      eyeIndexLimit = maybe
        (Array.length elements - 1)
        (_.eyeIndexLimit)
        (List.head frames)

    in
      case event of
        InputElementsCompared idx1 idx2 result → Right $
          { elementsAfter: elementsBefore
          , elementsBefore
          , event: InputElementsCompared idx1 idx2 result
          , eyeIndexAfter: eyeIndexBefore
          , eyeIndexBefore
          , eyeIndexLimit
          } : frames

        InputElementsSwapped idx1 idx2 →
          swapElements idx1 idx2 elementsBefore <#>
            \elementsAfter →
              { elementsBefore
              , elementsAfter
              , event: InputElementsSwapped idx1 idx2
              , eyeIndexAfter: eyeIndexBefore
              , eyeIndexBefore
              , eyeIndexLimit
              } : frames

        Printed s → Right $
          { elementsAfter: elementsBefore
          , elementsBefore
          , event: Printed s
          , eyeIndexAfter: eyeIndexBefore
          , eyeIndexBefore
          , eyeIndexLimit
          } : frames

        VariableSet "j" value → Right $
          if value < eyeIndexLimit then
            { elementsAfter: elementsBefore
            , elementsBefore
            , event: VariableSet "j" value
            , eyeIndexAfter: value
            , eyeIndexBefore
            , eyeIndexLimit
            } : frames
          else frames

        VariableSet "i" value → Right $
          { elementsAfter: elementsBefore
          , elementsBefore
          , event: VariableSet "i" value
          , eyeIndexAfter: eyeIndexBefore
          , eyeIndexBefore
          , eyeIndexLimit: value
          } : frames

        VariableSet name _ → Left
          $ "unknown variable name '" <> name <> "'"

  swapElements idx1 idx2 els = do
    val1 ← note "animation creation" $ els !! idx1
    val2 ← note "animation creation" $ els !! idx2
    pure $ Array.updateAtIndices [ idx1 /\ val2, idx2 /\ val1 ] els

eyeMovementKeyframes ∷ Int → CSS
eyeMovementKeyframes idx =
  let
    firstFrame = 0.0 /\ (CSS.left $ CSS.rem $ eyeLeft $ idx - 1)

    otherFrames =
      [ 100.0 /\ (CSS.left $ CSS.rem $ eyeLeft idx) ]
  in
    CSS.keyframes
      (eyeMovementIndexToString idx)
      (firstFrame :| otherFrames)

shiftKeyframes ∷ ShiftDirection → CSS
shiftKeyframes direction = CSS.keyframes
  (shiftDirectionToString direction)
  (firstFrame :| otherFrames)
  where
  firstFrame = 0.0 /\ do
    CSS.left $ CSS.rem 0.0
    CSS.top $ CSS.rem 0.0

  otherFrames =
    [ 30.0 /\ do
        CSS.left $ CSS.rem 0.0
        CSS.top $ CSS.rem delta
    , 70.0 /\ do
        CSS.left $ CSS.rem delta
        CSS.top $ CSS.rem delta
    , 100.0 /\ do
        CSS.left $ CSS.rem delta
        CSS.top $ CSS.rem 0.0
    ]

  delta = 8.0 * case direction of
    ShiftLeft →
      -1.0

    ShiftRight →
      1.0

eyeMovementAnimation ∷ Int → CSS
eyeMovementAnimation idx = CSS.animation
  (AnimationName $ CSS.fromString $ eyeMovementIndexToString idx)
  (CSS.sec 1.0)
  CSS.easeInOut
  (CSS.sec 0.0)
  (CSS.iterationCount 1.0)
  CSS.normalAnimationDirection
  CSS.forwards

shiftAnimation ∷ ShiftDirection → CSS
shiftAnimation direction = CSS.animation
  (AnimationName $ CSS.fromString $ shiftDirectionToString direction)
  (CSS.sec 1.0)
  CSS.easeInOut
  (CSS.sec 0.0)
  (CSS.iterationCount 1.0)
  CSS.normalAnimationDirection
  CSS.forwards

eyeLeft ∷ Int → Number
eyeLeft idx = 8.0 * (Int.toNumber idx + 0.5)
