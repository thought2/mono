module Test.Debug where

import Prelude
import Board (Board)
import Board as Board
import Common (Size, Step(..), Vec2)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import CrissCross (CrissCross)
import CrissCross as CrissCross
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Either as Either
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy as LList
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Effect.Exception as Exception
import Effect.Random (randomInt)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Random.LCG as RandomLCG
import Test.QuickCheck.Gen as Gen

sampleBoard :: Board
sampleBoard = Board.init (vec2 3 3)

prettyLog :: Board -> Effect Unit
prettyLog board = Console.log $ Board.prettyPrint board

prettyLog' :: forall err. Show err => Either err Board -> Effect Unit
prettyLog' eitherBoard = case eitherBoard of
  Left err -> Console.log (show err)
  Right board -> Console.log $ Board.prettyPrint board

-- READ TEXT FILE
data ErrReadTextFile
  = ErrReadTextFile { path :: String, nativeErr :: Exception.Error }

readTextFile :: String -> ExceptT ErrReadTextFile Effect String
readTextFile path =
  try (FS.readTextFile UTF8 path)
    # ExceptT
    # withExceptT (\nativeErr -> ErrReadTextFile { path, nativeErr })

-- RANDOM VEC 2
randomVec2 :: Vec2 Int -> Vec2 Int -> Effect (Vec2 Int)
randomVec2 start end =
  vec2
    <$> randomInt (start !! d0) (end !! d0)
    <*> randomInt (start !! d1) (end !! d1)

-- SHUFFLE
shuffle :: forall a. Array a -> Effect (Array a)
shuffle xs = do
  seed <- RandomLCG.randomSeed
  Gen.shuffle xs
    # Gen.sample seed 1
    # Array.head
    # fromMaybe []
    # pure

-- ADD RANDOM WORD
data ErrAddRandomWord
  = ErrAddRandomWordNotPossible

addRandomWord :: CrissCross -> ExceptT ErrAddRandomWord Effect CrissCross
addRandomWord crissCross = do
  let
    size = CrissCross.getSize crissCross
  positions <- lift $ shuffle $ _.position <$> CrissCross.getFields crissCross
  words <- lift $ shuffle $ CrissCross.getDict crissCross
  steps <- lift $ shuffle [ LeftRight, TopDown ]
  let
    all :: LList.List CrissCross
    all = do
      word <- LList.fromFoldable words
      position' <-
        LList.fromFoldable
          ( Array.filter
              ( \pos ->
                  ((pos !! d0) + String.length word <= (size !! d0))
                    && ((pos !! d1) + String.length word <= (size !! d1))
              )
              positions
          )
      step <- LList.fromFoldable steps
      let
        position = position' -- TODO: position' - (dir * (length word / 2))

        crossWord = { position, word, step }
      either mempty pure $ CrissCross.setWord crossWord crissCross
  all
    # LList.head
    # Either.note ErrAddRandomWordNotPossible
    # except

-- RUN
data ErrRun
  = ErrRunReadWords ErrReadTextFile
  | ErrRunTmp

run :: Config -> ExceptT ErrRun Effect String
run config =
  do
    content <- withExceptT ErrRunReadWords $ readTextFile (config.wordsPath)
    let
      words = String.split (Pattern "\n") content
    ( foldl (>>=)
        (pure $ CrissCross.init (config.size) words)
        (Array.replicate config.n $ (addRandomWord >>> withExceptT (const ErrRunTmp)))
    )
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
