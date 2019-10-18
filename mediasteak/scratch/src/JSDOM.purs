module JSDOM where

import Web.DOM.Document (Document)

foreign import getDocument :: String -> Document
