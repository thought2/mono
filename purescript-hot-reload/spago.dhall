{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "console"
    , "data-default"
    , "effect"
    , "nullable"
    , "psci-support"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
