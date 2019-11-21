{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "halogen-hor-reload"
, dependencies =
    [ "argonaut", "console", "effect", "hot-reload", "psci-support", "halogen" ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
