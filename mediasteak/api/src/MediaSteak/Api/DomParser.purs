module MediaSteak.Api.DomParser where

import Prelude
import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafeCrashWith)

data Error
  = ErrDomParse DOMReadOnly.Error
  | ErrStrParse StringParser.Error

parseNumberOfPages :: forall m element. element -> ExceptT Error m Int
parseNumberOfPages element = do
  url <-
    element
      # DomReadOnly.query ".wp-pagenavi a.last"
      >>= DomReadOnly.attribute "href"
      # withExceptT ErrDomParse
  StringParser.runParser parseUrl url
    # except
    # withExceptT ErrFooParseUrl

parseNumberOfPages' :: forall m element. element -> DOMReadOnlyT m Int
parseNumberOfPages' element = do
  limk <-
    element
      # DomReadOnly.query ".wp-pagenavi a.last"
  url <- DomReadOnly.attribute "href" link
  label <- DomReadOnly.text url
  StringParser.runParser parseUrl url
    # except
    # withExceptT DomReadOnly.Error

parseNumberOfPages' :: forall m element. element -> ExcepT m Int
parseNumberOfPages' element = do
  limk <-
    element
      # DomReadOnly.query ".wp-pagenavi a.last"
      # ExceptT
  url <-
    DomReadOnly.attribute "href" link
      # ExceptT
  label <-
    DomReadOnly.text url
      # pure
  StringParser.runParser parseUrl url
    # except
    # withExceptT DomReadOnly.Error
