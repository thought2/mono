{ name =
    "node-child-process-either"
, dependencies =
    [ "console", "effect", "node-child-process", "psci-support" ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
