{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mediasteak-api"
, dependencies =
    [ "console"
    , "effect"
    , "partial"
    , "psci-support"
    , "string-parsers"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}