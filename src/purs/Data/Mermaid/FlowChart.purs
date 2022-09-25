module Data.Mermaid.FlowChart (FlowChartDef(..), Orientation(..), Segment(..), toString) where

import Prelude

import Data.Array (foldMap)

data Orientation
  = TopToBottom
  | BottomToTop
  | RightToLeft
  | LeftToRight

data Segment = Line String | SubGraph String (Array Segment)

data FlowChartDef = FlowChartDef Orientation (Array Segment)

toString :: FlowChartDef -> String
toString = case _ of
  FlowChartDef orientation segments ->
    "flowchart "
      <> orientationToString orientation
      <> "\n"
      <> foldMap (\segment -> "    " <> segmentToString segment <> "\n") segments

segmentToString :: Segment -> String
segmentToString = case _ of
  Line s ->
    s

  SubGraph id segments ->
    "subgraph "
      <> id
      <> "\n"
      <> (foldMap (\segment -> "    " <> segmentToString segment) segments)
      <> "\nend"

orientationToString :: Orientation -> String
orientationToString = case _ of
  TopToBottom ->
    "TD"

  BottomToTop ->
    "BT"

  RightToLeft ->
    "RL"

  LeftToRight ->
    "LR"
