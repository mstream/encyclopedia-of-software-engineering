let searchTrie =
      { dependencies =
        [ "aff-promise"
        , "argonaut-codecs"
        , "argonaut-core"
        , "arrays"
        , "assert"
        , "bifunctors"
        , "codec"
        , "console"
        , "const"
        , "effect"
        , "enums"
        , "exceptions"
        , "foldable-traversable"
        , "foreign-object"
        , "functions"
        , "halogen-formless"
        , "halogen-store"
        , "integers"
        , "lists"
        , "ordered-collections"
        , "partial"
        , "prelude"
        , "psci-support"
        , "quickcheck"
        , "routing"
        , "routing-duplex"
        , "safe-coerce"
        , "slug"
        , "spec-quickcheck"
        , "strings"
        , "tuples"
        , "unfoldable"
        , "web-dom"
        , "web-html"
        ]
      , repo = "https://github.com/klntsky/purescript-search-trie.git"
      , version = "v1.0.0"
      }

in  { name = "encyclopedia of software engineering"
    , dependencies =
      [ "aff"
      , "aff-promise"
      , "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "codec"
      , "console"
      , "const"
      , "datetime"
      , "effect"
      , "either"
      , "enums"
      , "exceptions"
      , "foldable-traversable"
      , "foreign-object"
      , "functions"
      , "halogen"
      , "halogen-formless"
      , "halogen-store"
      , "integers"
      , "lists"
      , "maybe"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "quickcheck"
      , "routing"
      , "routing-duplex"
      , "safe-coerce"
      , "slug"
      , "spec"
      , "spec-quickcheck"
      , "strings"
      , "transformers"
      , "tuples"
      , "unfoldable"
      , "web-dom"
      , "web-html"
      ]
    , packages = ./packages.dhall with search-trie = searchTrie
    , sources = [ "src/purs/**/*.purs", "test/purs/**/*.purs" ]
    }
