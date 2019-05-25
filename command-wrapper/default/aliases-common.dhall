let CommandWrapper = ../Types.dhall

in    [ { alias = "h"
        , arguments = [] : List Text
        , command = "help" 
        }
      
      , { alias = "man"
        , arguments = [ "--man" ]
        , command = "help" 
        }
      
      , { alias = "cfg"
        , arguments = [] : List Text
        , command = "config" 
        }
      
      , { alias = "dhall"
        , arguments = [ "--dhall" ]
        , command = "config" 
        }

      , { alias = "dhall-repl"
        , arguments = ["--dhall-repl"]
        , command = "config"
        }
      ]
    : List CommandWrapper.SubcommandAlias
