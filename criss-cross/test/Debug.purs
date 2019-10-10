module Test.Debug where

import Prelude
import Common (Size)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, withExceptT)
import CrissCross as CrissCross
import CrissCross.Gen (addRandomWord, tryTimes)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple as Tuple
import Data.Vec (vec2)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Random.LCG (randomSeed)
import Test.QuickCheck.Gen as Gen

-- READ TEXT FILE
data ErrReadTextFile
  = ErrReadTextFile { path :: String, nativeErr :: Exception.Error }

readTextFile :: String -> ExceptT ErrReadTextFile Effect String
readTextFile path =
  try (FS.readTextFile UTF8 path)
    # ExceptT
    # withExceptT (\nativeErr -> ErrReadTextFile { path, nativeErr })

-- RUN
data ErrRun
  = ErrRunReadWords ErrReadTextFile
  | ErrRunTmp

run :: Config -> ExceptT ErrRun Effect String
run config = do
  content <- withExceptT ErrRunReadWords $ readTextFile (config.wordsPath)
  let
    words = String.split (Pattern "\n") content
  (CrissCross.init (config.size) words)
    # tryTimes addRandomWord config.n
    # mapExceptT
        ( \gen -> do
            seed <- randomSeed
            Gen.runGen gen { size: 1, newSeed: seed }
              # Tuple.fst
              # pure
        )
    # withExceptT (const ErrRunTmp)
    <#> CrissCross.prettyPrint

-- MAIN
type Config
  = { wordsPath :: String
    , size :: Size Int
    , n :: Int
    }

main' :: Config -> Effect Unit
main' config = do
  result <- runExceptT $ run config
  case result of
    Left error -> Console.log $ show error -- TODO: Console.log
    Right output -> Console.log output

main :: Effect Unit
main =
  main'
    { wordsPath: "./data/wordlist-shuffled.txt"
    , size: vec2 20 30
    , n: 3
    }

-- INSTANCE
derive instance genericErrRun :: Generic ErrRun _

derive instance genericErrReadTextFile :: Generic ErrReadTextFile _

instance showErrRun :: Show ErrRun where
  show = genericShow

instance showErrReadTextFile :: Show ErrReadTextFile where
  show = genericShow
