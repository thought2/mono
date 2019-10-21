module DOMReadOnly.Static
  ( Static
  , query
  ) where

import Prelude
import DOMReadOnly.Common (Error, Selector)
import DOMReadOnly.Dynamic as Dynamic
import Data.Either (Either)
import Effect.Unsafe (unsafePerformEffect)
import Web.DOM.Element (Element)

newtype Static a
  = Static a

query :: Selector -> Static Element -> Either Error (Static Element)
query selector (Static element) =
  Dynamic.query selector element
    # unsafePerformEffect
    <#> Static
