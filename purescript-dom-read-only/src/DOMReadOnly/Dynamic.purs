module DOMReadOnly.Dynamic
  ( query
  ) where

import Prelude
import Data.Either (Either)
import Data.Either as Either
import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element, Node)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as ParentNode
import DOMReadOnly.Common (Error(..), Selector)

query :: Selector -> Element -> Effect (Either Error Element)
query selector element =
  ParentNode.querySelector (QuerySelector selector) (Element.toParentNode element)
    <#> Either.note (ErrorQuery { selector, belowElement: Element.tagName element })
 {-
queryAll :: Selector -> Element -> Effect (Array Element)
queryAll selector element =
  ParentNode.querySelectorAll (QuerySelector selector) (Element.toParentNode element)
    >>= NodeList.toArray
    <#> (map (unsafeCoerce :: Node -> Element))

attribute :: String -> Element -> Effect (Either Error String)
attribute attr element =
  Element.getAttribute attr element
    <#> Either.note "ERR"

children :: Element -> Effect (Array Element)
children element =
  ParentNode.children (Element.toParentNode element)
    >>= HTMLCollection.toArray

text :: Element -> Effect String
text element = Node.textContent (Element.toNode element)
-}