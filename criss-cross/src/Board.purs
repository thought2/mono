module Board
  ( init
  , setWord
  , findWords
  , ErrSetWord(..)
  , prettyPrint
  , Cell(..)
  , Board
  ) where

import Prelude
import Common (CrossWord, Position, Size, Step(..), stepToDirection)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable (class Foldable, foldM)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple.Nested ((/\), type (/\))
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
findWord :: Position Int -> Step -> Board -> Maybe String
findWord posStart step board = case fields of
  [ Left _, Right _, Right _ ] -> Just $ go posStart ""
  _ -> Nothing
  where
  dir = stepToDirection step

  fields :: Array (Either ErrGetStone Char)
  fields =
    map
      ( \i ->
          getStone (posStart + dir * pure i) board
      )
      [ -1, 0, 1 ]

  go :: Position Int -> String -> String
  go pos xs = case getStone pos board of
    Right char -> go (pos + dir) (xs <> fromCharArray [ char ])
    Left _ -> xs

-- FIND WORDS
findWords :: Board -> Array CrossWord
findWords board =
  Matrix.toIndexedArray board
    >>= ( \{ position, value } ->
          mkCrossWord position LeftRight <> mkCrossWord position TopDown
      )
  where
  mkCrossWord :: Position Int -> Step -> Array CrossWord
  mkCrossWord position step =
    maybe []
      ( \word ->
          [ { position, step, word } ]
      )
      (findWord position step board)

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
                      Stone char -> String.toUpper $ fromCharArray [ char ]
                  )
              # String.joinWith ""
        )
    # String.joinWith "\n"

-- INSTANCE
derive instance genericErrSetWord :: Generic ErrSetWord _

instance showErrSetWord :: Show ErrSetWord where
  show = genericShow

derive instance genericCell :: Generic Cell _

instance showCell :: Show Cell where
  show = genericShow
