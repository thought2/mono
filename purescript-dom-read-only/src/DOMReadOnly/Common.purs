module DOMReadOnly.Common where

type Selector
  = String

data Error
  = ErrorQuery { selector :: Selector, belowElement :: String }
  | ErrorAttribute { attribute :: String, atElement :: String }
  | ErrorOther { message :: String }
