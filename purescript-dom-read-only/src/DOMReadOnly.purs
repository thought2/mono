module DOMReadOnly where

import Prelude
import Control.Monad.Except (ExceptT(..), except, throwError)
import DOMReadOnly.Common (Error(ErrorOther), Selector)
import DOMReadOnly.Dynamic as Dynamic
import DOMReadOnly.Static (Static)
import DOMReadOnly.Static as Static
import Data.Either (Either)
import Data.Either as Either
import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Web.DOM (Element)

type DOMReadOnlyT m a
  = ExceptT Error m a

fail :: forall m a. Monad m => String -> DOMReadOnlyT m a
fail message = throwError $ ErrorOther { message }

class
  ( Monad m
  ) <= DOMReadOnly m element | element -> m where
  query ::
    Selector -> element -> DOMReadOnlyT m element

{-
  queryAll ::
    Selector -> element -> DOMReadOnlyT m (Array element)
  attribute ::
    String -> element -> DOMReadOnlyT m String
  children ::
    element -> DOMReadOnlyT m (Array element)
  text ::
    element -> DOMReadOnlyT m String
-}
instance domReadOnlyDynamic ::
  DOMReadOnly Effect Element where
  query selector element = ExceptT $ Dynamic.query selector element

{-
  queryAll = Dynamic.queryAll
  attribute = Dynamic.attribute
  children = Dynamic.children
  text = pure $ Dynamic.text
  -}
instance domReadOnlyStatic ::
  DOMReadOnly Identity (Static Element) where
  query selector element = except $ Static.query selector element
