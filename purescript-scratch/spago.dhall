{ name =
    "purescript-scratch"
, dependencies =
    [ "effect", "console", "psci-support", "profunctor-lenses", "typelevel" ]
, packages =
    ./../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}