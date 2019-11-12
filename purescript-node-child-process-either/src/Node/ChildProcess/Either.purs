module Node.ChildProcess.Either
  ( execSync
  , module Node.ChildProcess
  , ExecSyncError
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Exception (try)
import Effect.Exception as Exception
import Node.Buffer (Buffer)
import Node.ChildProcess (ExecSyncOptions, defaultExecSyncOptions)
import Node.ChildProcess as ChildProcess
import Unsafe.Coerce (unsafeCoerce)

type ActualExecSyncError
  = { pid :: Int
    , output :: Array Buffer
    , stdout :: Buffer
    , stderr :: Buffer
    , status :: Nullable Int
    , signal :: Nullable String
    , error :: Exception.Error
    }

type ExecSyncError
  = { pid :: Int
    , output :: Array Buffer
    , stdout :: Buffer
    , stderr :: Buffer
    , status :: Maybe Int
    , signal :: Maybe String
    , error :: Exception.Error
    }

getExecSyncError :: ActualExecSyncError -> ExecSyncError
getExecSyncError obj =
  obj
    { status = Nullable.toMaybe obj.status
    , signal = Nullable.toMaybe obj.signal
    }

execSync :: String -> ExecSyncOptions -> Effect (Either ExecSyncError Buffer)
execSync x1 x2 =
  ChildProcess.execSync x1 x2
    # try
    <#> lmap (getExecSyncError <<< unsafeCoerce)
