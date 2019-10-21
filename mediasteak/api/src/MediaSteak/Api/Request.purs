module MediaSteak.Api.Request where

mock :: { domReadOnly :: { dynamic :: {} } }
mock = unsafeCrashWith ""

getNumberOfPages :: forall m. IO m => ExceptT ErrGet m Int
getNumberOfPages = do
  html <- get "https://www.mediasteak.com/aktuell/page/"
  let
    node = fromHtmlString html
  runExceptT $ parseNPages
