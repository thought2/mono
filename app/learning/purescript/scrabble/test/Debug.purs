module Test.Debug where

import Prelude
import Board (Board)
import Board as Board
import Common (Size, Step(..), Vec2, Position)
import Control.Monad.Except (ExceptT(..), except, lift, mapExceptT, runExceptT, withExceptT)
import CrissCross (CrissCross)
import CrissCross as CrissCross
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Either as Either
import Data.Foldable (foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy as LList
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple as Tuple
import Data.Typelevel.Num (d0, d1)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (vec2, (!!))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Effect.Exception as Exception
import Effect.Random (randomInt)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Partial.Unsafe (unsafePartial)
import Random.LCG (mkSeed, randomSeed)
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

addRandomWord :: CrissCross -> ExceptT ErrAddRandomWord Gen.Gen CrissCross
addRandomWord crissCross = do
  positions <- lift $ Gen.shuffle $ _.position <$> CrissCross.getFields crissCross
  words <- lift $ Gen.shuffle $ CrissCross.getDict crissCross
  steps <- lift $ Gen.shuffle [ LeftRight, TopDown ]
  arrayFindMap3
    words
    positions
    steps
    ( \word position step ->
        CrissCross.setWord { word, position, step } crissCross
          # Either.hush
    )
    # Either.note ErrAddRandomWordNotPossible
    # except

-- RUN
data ErrRun
  = ErrRunReadWords ErrReadTextFile
  | ErrRunTmp

run :: Config -> ExceptT ErrRun Effect String
run config = do
  content <- withExceptT ErrRunReadWords $ readTextFile (config.wordsPath)
  let
    words = String.split (Pattern "\n") content
  addRandomWord (CrissCross.init (config.size) words)
    # mapExceptT
        ( \gen -> do
            seed <- randomSeed
            Gen.runGen gen { size: 1, newSeed: seed }
              # Tuple.fst
              # pure
        )
    # withExceptT (const ErrRunTmp)
    <#> CrissCross.prettyPrint

-- TRY TIMES
data ErrTryTimes e a
  = ErrTryTimesNum Int e a

tryTimes :: forall a e. a -> (a -> Either e a) -> Int -> Either (ErrTryTimes e a) a
tryTimes init f n = go init n
  where
  go :: a -> Int -> Either (ErrTryTimes e a) a
  go x 0 = Right x

  go x i = case f x of
    Left e -> Left $ ErrTryTimesNum i e x
    Right x -> go x (i - 1)

-- ARRAY FIND 3
arrayFindMap3 ::
  forall a b c d.
  Array a -> Array b -> Array c -> (a -> b -> c -> Maybe d) -> Maybe d
arrayFindMap3 xs1 xs2 xs3 f =
  Array.findMap
    ( \x1 ->
        Array.findMap
          ( \x2 ->
              Array.findMap
                ( \x3 ->
                    f x1 x2 x3
                )
                xs3
          )
          xs2
    )
    xs1

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
