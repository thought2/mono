{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "browse-graph"
, dependencies =
    [ "argonaut"
    , "colehaus-graphs"
    , "console"
    , "debug"
    , "effect"
    , "psci-support"
    , "web-html"
    , "web-storage"
    ]
, packages = ../packages.dhall
, sources = [ "src/purs/**/*.purs", "test/**/*.purs" ]
}
