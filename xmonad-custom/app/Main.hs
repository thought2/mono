import           XMonad
import           XMonadCustom.Config    (makeConfig)
import           XMonadCustom.EnvConfig
import qualified XMonadCustom.EnvConfig

main :: IO ()
main = do
  envConfig <- XMonadCustom.EnvConfig.getUnsafe
  xmonad $ makeConfig envConfig
