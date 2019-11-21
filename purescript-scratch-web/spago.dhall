{ name =
    "purescript-scratch"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "halogen"
    , "halogen-hot-reload"
    , "psci-support"
    ]
, packages =
    ./../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
