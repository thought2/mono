{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mediasteak-api"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "generics-rep"
    , "partial"
    , "psci-support"
    , "quickcheck"
    , "string-parsers"
    , "transformers"
    ]
, packages =
    ./../../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
