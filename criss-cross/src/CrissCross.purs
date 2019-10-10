module CrissCross
  ( init
  , setWord
  , prettyPrint
  , ErrIsValid
  , CrissCross
  , ErrSetWord(..)
  , getWords
  , getBoard
  , getSize
  , getFields
  , getDict
  ) where

import Prelude
import Board (Board)
import Board as Board
import Common (CrossWord, Size, Position)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Matrix.Extra as Matrix

newtype CrissCross
  = CrissCross CrissCrossData

type CrissCrossData
  = { board :: Board, dict :: Set String }

-- INIT
initCleanWords :: Array String -> Array String
initCleanWords words =
  words
    # map String.trim
    # Array.filter isValidWord
  where
  isValidWord word = word /= ""

init :: Size Int -> Array String -> CrissCross
init size words =
  CrissCross
    $ { board: Board.init size
      , dict: Set.fromFoldable $ initCleanWords words
      }

-- GET BOARD
getBoard :: CrissCross -> Board
getBoard (CrissCross { board }) = board

-- SET WORD
data ErrSetWord
  = ErrSetWordOutside CrossWord
  | ErrSetWordMisfit CrossWord
  | ErrSetWordInvalid ErrIsValid
  | ErrSetWordNotInDict
  | ErrSetWordWordExists
  | ErrSetWordSingleChar

setWord ::
  CrossWord -> CrissCross -> Either ErrSetWord CrissCross
setWord crossWord@{ position, step, word } crissCross@(CrissCross { board, dict }) =
  if not $ Set.member word dict then
    Left ErrSetWordNotInDict
  else
    if Set.member word existingWords then
      Left ErrSetWordWordExists
    else
      if String.length word == 1 then
        Left ErrSetWordSingleChar
      else do
        case Board.setWord position step (toCharArray word) board of
          Left Board.ErrSetWordMisfit -> Left $ ErrSetWordMisfit crossWord
          Left Board.ErrSetWordOutside -> Left $ ErrSetWordOutside crossWord
          Right newBoard -> case isValid { dict, board: newBoard } of
            Right _ -> Right $ CrissCross { board: newBoard, dict }
            Left errIsValid -> Left $ ErrSetWordInvalid errIsValid
  where
  existingWords = Set.fromFoldable $ _.word <$> getWords crissCross

  checkWord :: String -> Boolean
  checkWord str =
    (not $ Set.member str existingWords)
      && Set.member str dict

-- GET WORDS
getWords :: CrissCross -> Array CrossWord
getWords crissCross = Board.findWords (getBoard crissCross)

-- GET SIZE
getSize :: CrissCross -> Size Int
getSize crissCross = Matrix.size $ getBoard crissCross

-- GET FIELDS
getFields :: CrissCross -> Array { position :: Position Int, value :: Maybe Char }
getFields crissCross =
  getBoard crissCross
    # Matrix.toIndexedArray
    # map
        ( \{ position, value } ->
            { position
            , value:
              case value of
                Board.Empty -> Nothing
                Board.Stone x -> Just x
            }
        )

-- GET DICT
getDict :: CrissCross -> Array String
getDict (CrissCross { dict }) = Set.toUnfoldable dict

-- PRETTY PRINT
prettyPrint :: CrissCross -> String
prettyPrint = getBoard >>> Board.prettyPrint

-- IS VALID
data ErrIsValid
  = ErrIsValidSingleChar
  | ErrIsValidWordNotInDict String
  | ErrIsValidWordTwice

isValid :: CrissCrossData -> Either ErrIsValid Unit
isValid { dict, board } =
  (foldr (\x acc -> acc *> checkWord x) (Right unit) words)
    *> if Array.nub words == words then
        Right unit
      else
        Left ErrIsValidWordTwice
  where
  checkWord word =
    if String.length word == 1 then
      Left ErrIsValidSingleChar
    else
      if not $ Set.member word wordsLookup then
        Left $ ErrIsValidWordNotInDict word
      else
        Right unit

  words = _.word <$> Board.findWords board

  wordsLookup = Set.fromFoldable dict

-- INSTANCE
derive instance genericErrSetWord :: Generic ErrSetWord _

derive instance genericErrIsValid :: Generic ErrIsValid _

instance showErrSetWord :: Show ErrSetWord where
  show = genericShow

instance showErrIsValid :: Show ErrIsValid where
  show = genericShow

instance showCrissCorss :: Show CrissCross where
  show crissCross =
    String.joinWith "\n"
      [ ""
      , "Dict: " <> showDict <> " ..."
      , "Words: " <> show (Board.findWords $ getBoard crissCross)
      , ""
      , prettyPrint crissCross
      , ""
      ]
    where
    showDict =
      getDict crissCross
        # Array.fromFoldable
        # Array.take 10
        # String.joinWith ", "
