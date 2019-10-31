{-# LANGUAGE DeriveGeneric #-}

module XMonadCustom.EnvConfig where

import           Data.Aeson           (FromJSON)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString
import           GHC.Generics
import           System.Environment   (getEnv)

data Tools = Tools
  { firefox      :: String
  , thunderbird  :: String
  , rofi         :: String
  , sleep        :: String
  , scrot        :: String
  , chromium     :: String
  , emacs        :: String
  , xterm        :: String
  , pavucontrol  :: String
  , nmtui        :: String
  , xdotool      :: String
  , i3lock       :: String
  , showKeyboard :: String
  , pactl        :: String
  } deriving (Show, Generic)

data Colors = Colors
  { border :: String
  } deriving (Show, Generic)

data EnvConfig = EnvConfig
  { tools  :: Tools
  , colors :: Colors
  } deriving (Show, Generic)

instance FromJSON EnvConfig

instance FromJSON Colors

instance FromJSON Tools

getUnsafe :: IO EnvConfig
getUnsafe = do
  configPath <- getEnv "CONFIG_PATH"
  content <- ByteString.readFile configPath
  case Aeson.eitherDecode content of
    Left err -> error $ show err
    Right ok -> pure ok
