{ name =
    "purescript-scratch"
, dependencies =
    [ "console"
    , "effect"
    , "profunctor-lenses"
    , "psci-support"
    , "record"
    , "typelevel"
    , "unsafe-coerce"
    ]
, packages =
    ./../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
