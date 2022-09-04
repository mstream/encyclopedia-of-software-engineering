let searchTrie =
      { dependencies =
        [ "arrays"
        , "assert"
        , "bifunctors"
        , "console"
        , "effect"
        , "exceptions"
        , "foldable-traversable"
        , "halogen-store"
        , "lists"
        , "ordered-collections"
        , "prelude"
        , "psci-support"
        , "routing"
        , "routing-duplex"
        , "safe-coerce"
        , "strings"
        ]
      , repo = "https://github.com/klntsky/purescript-search-trie.git"
      , version = "v1.0.0"
      }

in  { name = "encyclopedia of software engineering"
    , dependencies =
      [ "aff"
      , "arrays"
      , "console"
      , "datetime"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "functions"
      , "halogen"
      , "halogen-store"
      , "maybe"
      , "now"
      , "ordered-collections"
      , "prelude"
      , "routing"
      , "routing-duplex"
      , "safe-coerce"
      , "spec"
      , "strings"
      , "transformers"
      , "web-dom"
      , "web-html"
      ]
    , packages = ./packages.dhall with search-trie = searchTrie
    , sources = [ "src/purs/**/*.purs", "test/purs/**/*.purs" ]
    }
