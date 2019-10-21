module Test.MediaSteak.Api where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), except, mapExceptT, runExceptT, withExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity)
import Data.Int (odd)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe as RegexUnsafe
import Debug.Trace (spy)
import Effect (Effect)
import MediaSteak.Api (ErrGet, MediaItem)
import MediaSteak.Api as Api
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodeUnits (eof, string, regex)

newtype Mock a
  = Mock (Gen a)

newtype MockConfig
  = MockConfig { mediaItems :: Array MediaItem, nPages :: Int }

run :: forall a. ExceptT ErrGet Mock a -> Effect (Either Api.ErrGet a)
run m = (Gen.randomSample $ unwrap $ runExceptT m) <#> (\xs -> unsafePartial $ Array.unsafeIndex xs 0)

mock :: MockConfig -> String -> Either ErrGet String
mock (MockConfig { mediaItems, nPages }) path = do
  let
    _ = spy "nPages" nPages

    _ = spy "len" $ Array.length mediaItems

    parsePath =
      (string "aktuell/page/" *> parseInt <* string "/" <* eof)
        <|> (string "aktuell/" <* eof <#> (const 1))
  index <- runParser parsePath path # lmap (const Api.ErrGet)
  let
    startIndex = index * itemsPerPage

    endIndex = startIndex + itemsPerPage
  case Array.slice startIndex endIndex mediaItems of
    [] -> Left Api.ErrGet
    slice -> pure $ mockPage { mediaItems: slice, nPages }
  where
  itemsPerPage = Array.length mediaItems `div` nPages

  parseInt :: Parser Int
  parseInt =
    regex "\\d+"
      <#> Int.fromString
      >>= (maybe (StringParser.fail "Expected an integer") pure)

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

derive instance newtypeMock :: Newtype (Mock a) _

derive instance newtypeMockConfig :: Newtype MockConfig _

-- instance ioMock :: Api.IO Mock where
--   get url = pure "return"
instance ioMock :: Api.IO Mock where
  get url = mapExceptT Mock $ ExceptT $ arbitrary <#> \config -> mock config url

instance arbitraryMockConfig :: Arbitrary MockConfig where
  arbitrary = do
    mediaItems <- arbitrary
    nPages <- Gen.chooseInt 1 (Array.length mediaItems)
    pure $ MockConfig { mediaItems, nPages }
