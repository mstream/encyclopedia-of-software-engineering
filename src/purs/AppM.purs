module AppM where

import Prelude

import Capability.Navigate (class Navigate)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Route as Route
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Store (Action, Store)
import Store as Store

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM
  ∷ ∀ q i o
  . Store.Store
  → H.Component q i o AppM
  → Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadStore Action Store AppM

instance Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.codec

