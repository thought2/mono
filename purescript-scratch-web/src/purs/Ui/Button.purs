module Ui.Button (component) where

import Prelude
import Control.Monad.Free (liftF)
import Control.Monad.State (class MonadState)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Halogen (modify_, Component, mkComponent, mkEval, defaultEval, ComponentHTML, HalogenM) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HotReload (HalogenHotReloadM)
import Halogen.HotReload as HotReload
import Halogen.Query.HalogenM (HalogenF(..), HalogenM(..))
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)

type State
  = { enabled :: Boolean
    , id :: String
    , n :: Int
    }

data Action
  = Toggle
  | Up

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: HotReload.init HotReload.defaultInitConfig <<< initialState
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = unwrap <<< handleAction
            }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false, id: "foo", n: 0 }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "Onn" else "Off"
  in
    HH.button
      [ HP.title $ show state.n
      , HE.onClick \_ -> Just Up
      ]
      [ HH.text $ show state.n <> "!!!" ]

handleAction ∷ forall o m. Action → HalogenHotReloadM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st ->
      st { enabled = not st.enabled }
  Up ->
    H.modify_ \st ->
      st { n = st.n + 1 }
