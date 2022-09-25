module Data.Mermaid (DiagramDef(..), toString) where

import Data.Mermaid.FlowChart (FlowChartDef)
import Data.Mermaid.FlowChart as FlowChart

data DiagramDef = FlowChart FlowChartDef

toString :: DiagramDef -> String
toString = case _ of
  FlowChart def ->
    FlowChart.toString def

