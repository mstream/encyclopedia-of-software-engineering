let searchTrie =
      { dependencies =
        [ "arrays"
        , "assert"
        , "bifunctors"
        , "codec"
        , "console"
        , "effect"
        , "exceptions"
        , "foldable-traversable"
        , "halogen-store"
        , "lists"
        , "ordered-collections"
        , "partial"
        , "prelude"
        , "psci-support"
        , "routing"
        , "routing-duplex"
        , "safe-coerce"
        , "slug"
        , "strings"
        ]
      , repo = "https://github.com/klntsky/purescript-search-trie.git"
      , version = "v1.0.0"
      }

in  { name = "encyclopedia of software engineering"
    , dependencies =
      [ "aff"
      , "arrays"
      , "codec"
      , "datetime"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "halogen"
      , "halogen-store"
      , "maybe"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "routing"
      , "routing-duplex"
      , "safe-coerce"
      , "slug"
      , "spec"
      , "strings"
      , "transformers"
      ]
    , packages = ./packages.dhall with search-trie = searchTrie
    , sources = [ "src/purs/**/*.purs", "test/purs/**/*.purs" ]
    }
