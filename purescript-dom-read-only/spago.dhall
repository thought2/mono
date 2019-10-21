{ name =
    "dom-read-only"
, dependencies =
    [ "console", "effect", "psci-support", "transformers", "web-dom" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
