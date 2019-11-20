{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "hot-reload"
, dependencies =
    [ "argonaut", "console", "effect", "nullable", "psci-support" ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
