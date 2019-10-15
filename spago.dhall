{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff-promise"
    , "console"
    , "control"
    , "css"
    , "datetime"
    , "effect"
    , "foldable-traversable"
    , "foreign"
    , "foreign-generic"
    , "halogen"
    , "media-types"
    , "now"
    , "options"
    , "psci-support"
    , "routing"
    , "simple-json"
    , "string-parsers"
    , "strings"
    , "web-html"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
