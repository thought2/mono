module Halogen.HotReload (HalogenHotReloadM(..), init, class WithId, _id, module Exp) where

import Prelude
import Control.Monad.Cont (class MonadTrans)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Free (liftF)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (class MonadTell)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM(..))
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HotReload (InitConfig, defaultInitConfig) as Exp
import HotReload as HotReload
import Prim.Row as Row
import Record as Record
import Type.Prelude (class IsSymbol, class TypeEquals, SProxy(..))
import Type.Prelude as TypePrelude

class WithId a where
  _id :: a -> String

instance withIdRecord :: TypeEquals t { id :: String | r } => WithId t where
  _id :: t -> String
  _id = _.id <<< TypePrelude.to

init ::
  forall a.
  EncodeJson a => DecodeJson a => WithId a => Exp.InitConfig -> a -> a
init initConfig state = HotReload.unsafeInit (HotReload.Id $ _id state) initConfig state

newtype HalogenHotReloadM state action slots output m a
  = HalogenHotReloadM (HalogenM state action slots output m a)

instance monadStateHalogenM ::
  (EncodeJson state, DecodeJson state, WithId state) =>
  MonadState state (HalogenHotReloadM state action slots output m) where
  state f =
    HalogenHotReloadM
      $ HalogenM
      $ liftF
      $ Halogen.Query.HalogenM.State (map snapshot <<< f)
    where
    snapshot :: state -> state
    snapshot state' = HotReload.unsafeSnapshot (HotReload.Id $ _id state') state'

-- There must be a better way:
derive instance newtypeHalogenHotReloadM :: Newtype (HalogenHotReloadM state action slots output m a) _

derive newtype instance functorHalogenHotReloadM :: Functor (HalogenHotReloadM state action slots output m)

derive newtype instance applyHalogenHotReloadM :: Apply (HalogenHotReloadM state action slots output m)

derive newtype instance applicativeHalogenHotReloadM :: Applicative (HalogenHotReloadM state action slots output m)

derive newtype instance bindHalogenHotReloadM :: Bind (HalogenHotReloadM state action slots output m)

derive newtype instance monadHalogenHotReloadM :: Monad (HalogenHotReloadM state action slots output m)

derive newtype instance monadEffectHalogenHotReloadM :: MonadEffect m => MonadEffect (HalogenHotReloadM state action slots output m)

derive newtype instance monadAffHalogenHotReloadM :: MonadAff m => MonadAff (HalogenHotReloadM state action slots output m)

-- What?
-- derive newtype instance parallelHalogenHotReloadM :: Parallel (HalogenAp state action slots output m) (HalogenHotReloadM state action slots output m)
derive newtype instance monadTransHalogenHotReloadM :: MonadTrans (HalogenHotReloadM state action slots o)

derive newtype instance monadRecHalogenHotReloadM :: MonadRec (HalogenHotReloadM state action slots output m)

derive newtype instance monadAskHalogenHotReloadM :: MonadAsk r m => MonadAsk r (HalogenHotReloadM state action slots output m)

derive newtype instance monadTellHalogenHotReloadM :: MonadTell w m => MonadTell w (HalogenHotReloadM state action slots output m)

derive newtype instance monadThrowHalogenHotReloadM :: MonadThrow e m => MonadThrow e (HalogenHotReloadM state action slots output m)
