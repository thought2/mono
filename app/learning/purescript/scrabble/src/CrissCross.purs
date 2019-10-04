module CrissCross
  ( init
  , setWord
  , CrissCross
  , CrossWord
  , ErrSetWord(..)
  , getWords
  , getBoard
  ) where

import Prelude
import Board (Board)
import Board as Board
import Common (Position, Size, Step)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Typelevel.Undefined (undefined)

newtype CrissCross
  = CrissCross { board :: Board, words :: Set String }

type CrossWord
  = { position :: Position Int, step :: Step, word :: String }

init :: Size Int -> Array String -> CrissCross
init size words =
  CrissCross
    $ { board: Board.init size
      , words: Set.fromFoldable words
      }

-- GET BOARD
getBoard :: CrissCross -> Board
getBoard (CrissCross { board }) = board

-- SET WORD
data ErrSetWord
  = ErrSetWordOutside CrossWord
  | ErrSetWordMisfit CrossWord
  | ErrSetWordInvalid CrossWord

setWord ::
  forall f.
  FunctorWithIndex Int f =>
  Foldable f =>
  CrossWord -> CrissCross -> Either ErrSetWord CrissCross
setWord crossWord@{ position, step, word } (CrissCross { board, words }) = do
  case Board.setWord position step (toCharArray word) board of
    Left Board.ErrSetWordOutside -> Left $ ErrSetWordOutside crossWord
    Left Board.ErrSetWordMisfit -> Left $ ErrSetWordMisfit crossWord
    Right newBoard -> case Board.isValid checkWord newBoard of
      Right _ -> Right $ CrissCross { board: newBoard, words }
      Left _ -> Left $ ErrSetWordInvalid crossWord
  where
  checkWord :: Array Char -> Boolean
  checkWord xs = Set.member (fromCharArray xs) words

-- INSTANCE
derive instance genericErrSetWord :: Generic ErrSetWord _

instance showErrSetWord :: Show ErrSetWord where
  show = genericShow

-- TODO
getWords :: CrissCross -> Array CrossWord
getWords = undefined
