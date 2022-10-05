module Main (main) where

import Prelude

import AppM (runAppM)
import Component.Router (Query(..))
import Component.Router as Router
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex as RD
import Routing.Hash as RH

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  rootComponent ← runAppM initialStore Router.component
  halogenIO ← runUI rootComponent unit body
  void $ liftEffect $ RH.matchesWith (RD.parse Route.codec) \old new →
    when (old /= Just new) $ launchAff_ do
      void $ halogenIO.query $ H.mkTell $ Navigate new
  where
  initialStore = {}

