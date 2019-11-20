{ name =
    "purescript-scratch"
, dependencies =
    [ "console", "effect", "hot-reload", "psci-support" ]
, packages =
    ./../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
