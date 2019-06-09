let CommandWrapper = ../Types.dhall

in    [ { alias = "h"
        , description = None Text
        , arguments = [] : List Text
        , command = "help" 
        }
      
      , { alias = "man"
        , description = None Text
        , command = "help" 
        , arguments = [ "--man" ]
        }
      
      , { alias = "cfg"
        , description = None Text
        , command = "config" 
        , arguments = [] : List Text
        }
      
      , { alias = "dhall"
        , description = None Text
        , command = "config" 
        , arguments = [ "--dhall" ]
        }

      , { alias = "dhall-repl"
        , description = None Text
        , command = "config"
        , arguments = ["--dhall-repl"]
        }
      ]
    : List CommandWrapper.SubcommandAlias
