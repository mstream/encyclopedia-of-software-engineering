module Data.Route (Route(..), codec) where

import Prelude hiding ((/))

import Data.Article (Article)
import Data.Article as Article
import Data.Article.Encryption as Encryption
import Data.Codec as Codec
import Data.Generic.Rep (class Generic)
import Data.Set as Set
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route = Article Article | Home

derive instance Eq Route

codec :: RouteDuplex' Route
codec = root $ sum
  { "Article": "articles" / article segment
  , "Home": noArgs
  }

derive instance Generic Route _

article :: RouteDuplex' String -> RouteDuplex' Article
article = as
  (Codec.encode $ Article.codec articles)
  (Codec.decode $ Article.codec articles)
  where
  articles = Set.fromFoldable
    [ Encryption.article ]
