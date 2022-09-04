module Data.Route where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route = Home

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  }

derive instance Generic Route _
