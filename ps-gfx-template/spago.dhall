{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "canvas"
    , "colors"
    , "console"
    , "drawing"
    , "effect"
    , "integers"
    , "psci-support"
    , "web-html"
    ]
, packages =
    ./../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
