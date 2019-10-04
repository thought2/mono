module Board
  ( init
  , setWord
  , ErrSetWord(..)
  , prettyPrint
  , Cell
  , isValid
  , ErrIsValid
  , Board
  ) where

import Prelude
import Common (Direction, Size, Step(..), Position)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable (class Foldable, foldM, foldr)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec (vec2)
import Matrix.Extra (Matrix)
import Matrix.Extra as Matrix

type Board
  = Matrix Cell

data Cell
  = Empty
  | Stone Char

-- INIT
init :: Size Int -> Board
init size = Matrix.repeat size Empty

-- SET STONE
data ErrSetStone
  = ErrSetStoneOutside
  | ErrSetStoneTakenSame
  | ErrSetStoneTakenDiff

setStone :: Position Int -> Char -> Board -> Either ErrSetStone Board
setStone pos x mat = case getStone pos mat of
  Left ErrGetStoneOutside -> Left ErrSetStoneOutside
  Left ErrGetStoneInsideEmpty -> Either.note ErrSetStoneOutside (Matrix.set pos (Stone x) mat)
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
  Just Empty -> Left ErrGetStoneInsideEmpty
  Just (Stone char) -> Right char

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
findWord :: Position Int -> Step -> Board -> Maybe (Array Char)
findWord posStart step board = case fields of
  [ Left _, Right _, Right _ ] -> Just $ go posStart []
  _ -> Nothing
  where
  dir = stepToDirection step

  fields :: Array (Either ErrGetStone Char)
  fields =
    map
      ( \i ->
          getStone (posStart + dir * pure i) board
      )
      $ Array.range (-1) 1

  go :: Position Int -> Array Char -> Array Char
  go pos xs = case getStone pos board of
    Right char -> go (pos + dir) (xs <> [ char ])
    Left _ -> xs

findWords :: Board -> Array { position :: Position Int, step :: Step, word :: Array Char }
findWords board =
  Matrix.toIndexedArray board
    >>= ( \{ position, value } ->
          foo position LeftRight <> foo position TopDown
      )
  where
  foo :: Position Int -> Step -> Array { position :: Position Int, step :: Step, word :: Array Char }
  foo position step =
    maybe []
      ( \word ->
          [ { position, step, word } ]
      )
      (findWord position step board)

-- IS VALID
data ErrIsValid
  = ErrIsValidWordNotExist (Array Char)

isValid :: (Array Char -> Boolean) -> Board -> Either ErrIsValid Unit
isValid checkWord board =
  foldr
    ( \x acc ->
        acc *> checkField x.position
    )
    (pure unit)
    (Matrix.toIndexedArray board)
  where
  checkField :: Position Int -> Either ErrIsValid Unit
  checkField pos = checkFieldDir pos LeftRight <* checkFieldDir pos TopDown

  checkFieldDir :: Position Int -> Step -> Either ErrIsValid Unit
  checkFieldDir pos step = case findWord pos step board of
    Nothing -> pure unit
    Just word
      | checkWord word -> pure unit
    Just word -> Left $ ErrIsValidWordNotExist word

-- PRETTY PRINT
prettyPrint :: Board -> String
prettyPrint board =
  Matrix.toArrays board
    # map
        ( \row ->
            row
              # map
                  ( \cell -> case cell of
                      Empty -> "."
                      Stone char -> fromCharArray [ char ]
                  )
              # String.joinWith ""
        )
    # String.joinWith "\n"

-- INSTANCE
derive instance genericErrIsValid :: Generic ErrIsValid _

derive instance genericErrSetWord :: Generic ErrSetWord _

instance showErrIsValid :: Show ErrIsValid where
  show = genericShow

instance showErrSetWord :: Show ErrSetWord where
  show = genericShow
