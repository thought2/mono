{ name =
    "purescript-scratch"
, dependencies =
    [ "effect", "console", "psci-support" ]
, packages =
    ./../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}