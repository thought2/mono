module Test.MediaSteak.Api where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, except, withExceptT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity)
import Data.Int (odd)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe as RegexUnsafe
import MediaSteak.Api (ErrGet, MediaItem)
import MediaSteak.Api as Api
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodeUnits (eof, string, regex)

newtype Mock a
  = Mock (Identity a)

type MockConfig
  = { mediaItems :: Array MediaItem, nPages :: Int }

mockParseInt :: Parser Int
mockParseInt = regex "\\d+" >>= (maybe (StringParser.fail "Expected an integer.") pure <<< Int.fromString)

mock :: MockConfig -> String -> ExceptT ErrGet Mock String
mock { mediaItems, nPages } path = do
  let
    parsePath =
      (string "aktuell/page/" *> mockParseInt <* string "/" <* eof)
        <|> (string "aktuell/" <* eof <#> (const 1))
  index <- runParser parsePath path # except # withExceptT (const Api.ErrGet)
  let
    startIndex = index * itemsPerPage

    endIndex = startIndex + itemsPerPage
  case Array.slice startIndex endIndex mediaItems of
    [] -> except $ Left Api.ErrGet
    slice -> pure $ mockPage { mediaItems: slice, nPages }
  where
  itemsPerPage = Array.length mediaItems `div` nPages

mockPage :: { mediaItems :: Array MediaItem, nPages :: Int } -> String
mockPage { mediaItems, nPages } =
  String.joinWith "\n"
    $ ( mapWithIndex
          ( \index mediaItem ->
              ( if odd index then
                  mockItemSmall
                else
                  mockItemLarge
              )
                mediaItem
          )
          mediaItems
      )
    <> [ mockIndex { nPages } ]

mockIndex :: { nPages :: Int } -> String
mockIndex { nPages } =
  """
    <div class="wp-pagenavi" role="navigation">
      <a class="last" href="https://www.mediasteak.com/aktuell/page/${nPages}/">»
      </a>
    </div>
  """
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{nPages\\}" RegexFlags.noFlags) (show nPages)

mockItemSmall :: MediaItem -> String
mockItemSmall { title, url, description, duration, image } =
  """
    <div class="item large-4 medium-6 columns">
      <a href="${url}">
        <div class="box-wrapper">
          <div class="  corner-label text-center">
            <span>
            </span>
          </div>
          <div class="small-layover">
            <div class="top-bar">
              <h2>${title}
              </h2>
              <span class="hide-for-small-only text-right"> ${duration} min
              </span>
            </div>
            <div class="copy-wrapper">
              <p>${description}
              </p>
            </div>
          </div>
          <div class="content " style="background-image: url('${image}')">
          </div>
        </div>
      </a>
    </div>
  """
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{title\\}" RegexFlags.noFlags) title
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{url\\}" RegexFlags.noFlags) url
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{description\\}" RegexFlags.noFlags) description
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{duration\\}" RegexFlags.noFlags) (show duration)
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{image\\}" RegexFlags.noFlags) image

mockItemLarge :: MediaItem -> String
mockItemLarge { title, url, description, duration, image } =
  """
    <div class="item2 big large-8 medium-12 small-12 columns">
      <a href="${url}">
        <div class="box-wrapper">
          <div class="  corner-label text-center">
            <span>
            </span>
          </div>
          <div class="small-layover">
            <div class="top-bar">
              <h2>${title}
              </h2>
              <span class="hide-for-small-only text-right"> ${duration} min
              </span>
            </div>
            <div class="copy-wrapper">
              <p>${description}
              </p>
            </div>
          </div>
          <div class="content" style="background-image: url('${image}')">
          </div>
        </div>
      </a>
    </div>
  """
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{title\\}" RegexFlags.noFlags) title
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{url\\}" RegexFlags.noFlags) url
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{description\\}" RegexFlags.noFlags) description
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{duration\\}" RegexFlags.noFlags) (show duration)
    # Regex.replace (RegexUnsafe.unsafeRegex "\\$\\{image\\}" RegexFlags.noFlags) image

-- INSTANCES
derive newtype instance monadMock :: Monad Mock

derive newtype instance bindMock :: Bind Mock

derive newtype instance applicativeMock :: Applicative Mock

derive newtype instance functorMock :: Functor Mock

instance ioMock :: Api.IO Mock where
  get url = pure "return"
