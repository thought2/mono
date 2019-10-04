module Board (init, setWord, ErrSetWord(..), Step(..), pretty, prettyLog) where

import Prelude
import Common (Position, Size, Direction)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable (class Foldable, all, foldM)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec (vec2)
import Effect (Effect)
import Effect.Console as Console
import Matrix.Extra (Matrix)
import Matrix.Extra as Matrix

type Board
  = Matrix Cell

type Cell
  = Maybe Char

data Cell' =
  Empty | Stone Char

data Step
  = LeftRight
  | TopDown


-- INIT
init :: Size Int -> Board
init size = Matrix.repeat size Nothing

-- SET STONE
data ErrSetStone
  = ErrSetStoneOutside
  | ErrSetStoneTakenSame
  | ErrSetStoneTakenDiff

setStone :: Position Int -> Char -> Board -> Either ErrSetStone Board
setStone pos x mat = case getStone pos mat of
  Left ErrGetStoneOutside -> Left ErrSetStoneOutside
  Left ErrGetStoneInsideEmpty -> Either.note ErrSetStoneOutside (Matrix.set pos (Just x) mat)
  Right stone
    | stone == x -> Left ErrSetStoneTakenSame
  Right stone -> Left ErrSetStoneTakenDiff

-- GET STONE
data ErrGetStone
  = ErrGetStoneOutside
  | ErrGetStoneInsideEmpty

getStone :: Position Int -> Board -> Either ErrGetStone Char
getStone pos board = case Matrix.get pos board of
  Nothing -> Left ErrGetStoneOutside
  Just Nothing -> Left ErrGetStoneInsideEmpty
  Just (Just stone) -> Right stone

-- SET WORD
data ErrSetWord
  = ErrSetWordOutside
  | ErrSetWordMisfit

stepToDirection :: Step -> Direction Int
stepToDirection = case _ of
  LeftRight -> vec2 1 0
  TopDown -> vec2 0 1

setWord ::
  forall f.
  FunctorWithIndex Int f =>
  Foldable f =>
  Position Int -> Step -> f Char -> Board -> Either ErrSetWord Board
setWord startPos step startWord startBoard = foldM trySetStone startBoard indexedWord
  where
  indexedWord = mapWithIndex (/\) startWord

  dir = stepToDirection step

  getPos :: Int -> Position Int
  getPos index = startPos + (dir * pure index)

  trySetStone :: Board -> (Int /\ Char) -> Either ErrSetWord Board
  trySetStone board (index /\ x) = case setStone (getPos index) x board of
    Left ErrSetStoneOutside -> Left ErrSetWordOutside
    Left ErrSetStoneTakenSame -> Right board
    Left ErrSetStoneTakenDiff -> Left ErrSetWordMisfit
    Right newBoard -> Right newBoard

-- FIND WORD
findWord :: Position Int -> Board -> {leftRight :: Maybe (Array Char), topDown :: Maybe (Array Char)}
findWord pos board =
  { leftRight : f LeftRight
  , topDown : f TopDown
  }
  where
    f dir = case {here: Matrix.get pos board, prev: Matrix.get (pos - dir)} of
      { here : Just (Just _), prev : Nothing , next : Just (Just _) } ->
      { here : Just (Just _), prev : Just Nothing , next : Just (Just _)} ->

  case { here: Matrix.get pos board

  map (\x -> go pos False) (Matrix.get pos board)
  where
    go pos b =


-- IS VALID
data ErrIsValid
  = ErrIsValidWordNotExist (Array Char)

isValid :: (Array Char -> Boolean) -> Board -> Either ErrIsValid Unit
isValid checkWord board = all isValidCell $ Matrix.toIndexedArray board
  where
  isValidCell { pos, value } = case findWord pos board of
    Nothing -> pure unit
    Just word
      | checkWord word -> pure unit
    Just word -> Left $ ErrIsValidWordNotExist word

-- PRETTY
pretty :: Board -> String
pretty board =
  Matrix.toArrays board
    # map
        ( \row ->
            row
              # map (\mayCell -> maybe "." (pure >>> fromCharArray) mayCell)
              # String.joinWith ""
        )
    # String.joinWith "\n"

prettyLog :: Board -> Effect Unit
prettyLog board = Console.log $ pretty board
