module Main where

import Prelude
import Cheerio (Cheerio)
import Cheerio as Cheerio
import Cheerio.Static as CheerioStatic
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import JSDOM as JSDOM
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element, Node, ParentNode)
import Web.DOM.Document (Document)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as ParentNode
import Web.DOM.Text (Text)
import Web.DOM.Text as Text
import Web.HTML.HTMLAnchorElement as HTMLAnchorElement

sampleItem :: String
sampleItem =
  """<a href="https://www.mediasteak.com/mein-bruder-der-terrorist/">
            <div class="box-wrapper">
            <div class="  corner-label text-center"><span> </span></div>
                    <div class="small-layover">
                            <div class="top-bar"><h2>Mein Bruder, der Terrorist – zu Fuß gegen den Hass</h2> <span class="hide-for-small-only text-right"> 30 min</span></div>
                            <div class="copy-wrapper"><p>Von Marseille nach Paris: Abdelghani Merah wandert durch die Französische Republik – mit einer schweren Last und einer klaren Mission.</p></div>
                    </div>
                    <div class="content " style="background-image: url('https://www.mediasteak.com/wp-content/uploads/mediasteak-mein-bruder-der-terrorist-350x200.jpg')"></div>
            </div>
    </a>"""

class
  Monad m <= C m node where
  getNode :: String -> ExceptT String m node
  querySelector :: String -> node -> ExceptT String m node
  getAttribute :: String -> node -> ExceptT String m String
  getText :: node -> ExceptT String m String

unsafeQuerySelector :: QuerySelector -> ParentNode -> Maybe Element
unsafeQuerySelector querySelector parentNode =
  unsafePerformEffect
    $ ParentNode.querySelector querySelector parentNode

unsafeTextContent :: Node -> String
unsafeTextContent node =
  unsafePerformEffect
    $ Node.textContent node

unsafeGetAttribute :: String -> Element -> Maybe String
unsafeGetAttribute attrName element =
  unsafePerformEffect
    $ Element.getAttribute attrName element

instance cId :: C Identity StaticNode where
  getNode str =
    JSDOM.getDocument str
      # Document.toParentNode
      # Right
      # pure
      # ExceptT
      <#> StaticNode
  querySelector selector (StaticNode parentNode) =
    unsafeQuerySelector (QuerySelector selector) parentNode
      # Either.note ("Cannot select " <> selector)
      <#> Element.toParentNode
      # pure
      # ExceptT
      <#> StaticNode
  getAttribute attrName (StaticNode parentNode) =
    Element.fromParentNode parentNode
      # Either.note "C1"
      >>= (Either.note "C2" <<< unsafeGetAttribute attrName)
      # pure
      # ExceptT
  getText (StaticNode parentNode) =
    Element.fromParentNode parentNode
      # Either.note "D1"
      <#> Element.toNode
      # pure
      # ExceptT
      <#> unsafeTextContent

newtype StaticNode
  = StaticNode ParentNode

parseItem' :: forall m node. C m node => node -> ExceptT String m Item
parseItem' node = do
  url <-
    querySelector "a" node
      >>= getAttribute "href"
  title <-
    querySelector "h2" node
      >>= getText
  description <-
    querySelector ".copy-wrapper p" node
      >>= getText
  pure { url, title, description }

parseItem :: ParentNode -> MaybeT Effect Item
parseItem parentNode = do
  url <-
    parentNode
      # (MaybeT <<< ParentNode.querySelector (QuerySelector "a"))
      >>= (MaybeT <<< Element.getAttribute "href")
  title <-
    parentNode
      # (MaybeT <<< ParentNode.querySelector (QuerySelector "h2"))
      <#> Element.toNode
      >>= (MaybeT <<< pure <<< Text.fromNode)
      >>= (MaybeT <<< map Just <<< Text.wholeText)
  description <-
    parentNode
      # (MaybeT <<< ParentNode.querySelector (QuerySelector ".copy-wrapper p"))
      <#> Element.toNode
      >>= (MaybeT <<< pure <<< Text.fromNode)
      >>= (MaybeT <<< map Just <<< Text.wholeText)
  pure { url, title, description }

parseItems :: ParentNode -> MaybeT Effect (Array Item)
parseItems parentNode = do
  items :: Array Node <-
    ( ParentNode.querySelectorAll (QuerySelector "item") parentNode
        >>= NodeList.toArray
    )
      # lift
  traverse
    ( \node -> do
        element <- MaybeT $ pure $ Element.fromNode node
        parseItem (Element.toParentNode element)
    )
    items

type Item
  = { url :: String
    , title :: String
    , description :: String
    }

main :: Effect Unit
main = do
  log "🍝"
  let
    result =
      unwrap
        $ runExceptT do
            node <- (getNode sampleItem) :: ExceptT String Identity StaticNode
            parseItem' node
  case result of
    Right ok -> log $ show ok
    Left err -> log err
