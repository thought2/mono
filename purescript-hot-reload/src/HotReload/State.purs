module HotReload.State
  ( Fns
  , ConfigGetFns
  , defaultConfigGetFns
  , getFns
  , module HotReload
  ) where

import Prelude
import Control.Monad.State (class MonadState)
import Control.Monad.State as State
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Effect.Unsafe (unsafePerformEffect)
import HotReload
  ( Id(..)
  , InitConfig
  , defaultInitConfig
  , init
  , unsafeSnapshot
  )
  as HotReload
import HotReload (unsafeSnapshot)

type Fns
  = forall a.
    DecodeJson a =>
    EncodeJson a =>
    { init :: a -> a
    , modify :: forall m. MonadState a m => (a -> a) -> m a
    , modify_ :: forall m. MonadState a m => (a -> a) -> m Unit
    }

type ConfigGetFns
  = HotReload.InitConfig

defaultConfigGetFns :: ConfigGetFns
defaultConfigGetFns = HotReload.defaultInitConfig

getFns :: HotReload.Id -> ConfigGetFns -> Fns
getFns id config =
  { init:
    unsafePerformEffect <<< HotReload.init id config
  , modify:
    \f ->
      State.modify (HotReload.unsafeSnapshot id <<< f)
  , modify_:
    \f ->
      State.modify_ (HotReload.unsafeSnapshot id <<< f)
  }
