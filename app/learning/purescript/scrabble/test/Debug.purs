module Test.Debug where

import Prelude
import Board (Board)
import Board as Board
import Common (Size, Step(..), Vec2, CrossWord)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import CrissCross (CrissCross)
import CrissCross as CrissCross
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy as LList
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
shuffle xs = pure xs -- TODO: add shuffle

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
    allCrossWords :: LList.List CrossWord
    allCrossWords = do
      word <- LList.fromFoldable words
      position' <- LList.fromFoldable positions
      step <- LList.fromFoldable steps
      let
        position = position' -- TODO: position' - (dir * (length word / 2))
      pure { position, word, step }
  allCrossWords
    # LList.findMap
        ( \crossWord ->
            Either.hush $ CrissCross.setWord crossWord crissCross
        )
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
      words = Array.take 1000 $ String.split (Pattern "\n") content
    ( CrissCross.init (config.size) words
        # (addRandomWord >>> withExceptT (const ErrRunTmp))
        >>= (addRandomWord >>> withExceptT (const ErrRunTmp))
        >>= (addRandomWord >>> withExceptT (const ErrRunTmp))
    )
    <#> CrissCross.prettyPrint

-- MAIN
type Config
  = { wordsPath :: String
    , size :: Size Int
    }

main' :: Config -> Effect Unit
main' config = do
  result <- runExceptT $ run config
  case result of
    Left error -> Console.log $ show error -- TODO: Console.log
    Right output -> Console.log output

main :: Effect Unit
main = main' { wordsPath: "./data/wordlist-shuffled.txt", size: vec2 20 30 }

-- INSTANCE
derive instance genericErrRun :: Generic ErrRun _

derive instance genericErrReadTextFile :: Generic ErrReadTextFile _

instance showErrRun :: Show ErrRun where
  show = genericShow

instance showErrReadTextFile :: Show ErrReadTextFile where
  show = genericShow
