{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "criss-cross"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "matrices"
    , "maybe"
    , "minibench"
    , "node-fs"
    , "node-process"
    , "psci-support"
    , "string-parsers"
    , "test-unit"
    , "tuples"
    , "typelevel"
    , "vectors"
    ]
, packages =
    ./../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}