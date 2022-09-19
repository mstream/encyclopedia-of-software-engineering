module Data.Route (Route(..), codec) where

import Prelude hiding ((/))

import Data.ArticleId (ArticleId)
import Data.ArticleId as ArticleId
import Data.Codec as Codec
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route = Article ArticleId | Home

derive instance Eq Route

codec :: RouteDuplex' Route
codec = root $ sum
  { "Article": "articles" / articleId segment
  , "Home": noArgs
  }

derive instance Generic Route _

articleId :: RouteDuplex' String -> RouteDuplex' ArticleId
articleId = as
  (Codec.encode ArticleId.codec)
  (Codec.decode ArticleId.codec)
