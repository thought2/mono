{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "browse-graph"
, dependencies =
    [ "argonaut"
    , "colehaus-graphs"
    , "console"
    , "effect"
    , "psci-support"
    , "sized-vectors"
    , "web-html"
    , "web-storage"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
