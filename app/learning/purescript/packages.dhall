let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190920/packages.dhall sha256:53873cf2fc4a343a41f335ee47c1706ecf755ac7c5a336e8eb03ad23165dfd28

let overrides =
      { criss-cross =
          ./scrabble/spago.dhall as Location
      , halogen =
          { dependencies =
              [ "free"
              , "ordered-collections"
              , "console"
              , "profunctor"
              , "media-types"
              , "profunctor-lenses"
              , "fork"
              , "halogen-vdom"
              , "web-uievents"
              , "coroutines"
              , "aff"
              , "unsafe-reference"
              , "avar"
              , "dom-indexed"
              , "unsafe-coerce"
              , "const"
              , "freeap"
              , "transformers"
              , "parallel"
              , "foreign"
              , "nullable"
              ]
          , repo =
              "https://github.com/slamdata/purescript-halogen"
          , version =
              "v5.0.0-rc.4"
          }
      , halogen-vdom =
          { dependencies =
              [ "prelude"
              , "effect"
              , "tuples"
              , "web-html"
              , "foreign-object"
              , "maybe"
              , "unsafe-coerce"
              , "bifunctors"
              , "refs"
              , "foreign"
              ]
          , repo =
              "https://github.com/slamdata/purescript-halogen-vdom"
          , version =
              "v6.1.0"
          }
      }

let additions =
      { vectors =
          { dependencies =
              [ "arrays"
              , "console"
              , "distributive"
              , "effect"
              , "foldable-traversable"
              , "maybe"
              , "prelude"
              , "psci-support"
              , "test-unit"
              , "unfoldable"
              , "typelevel"
              ]
          , repo =
              "https://github.com/thought2/purescript-vectors.git"
          , version =
              "v1.0.0"
          }
      }

in  upstream ⫽ overrides ⫽ additions
