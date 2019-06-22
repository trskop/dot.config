let CommandWrapper = ../Types.dhall

in    [ { alias = "h"
        , description = Some "Short hand for \"help\"."
        , arguments = [] : List Text
        , command = "help"
        }

      , { alias = "man"
        , description = Some "Short hand for \"help --man\"."
        , command = "help"
        , arguments = [ "--man" ]
        }

      , { alias = "cfg"
        , description = Some "Short hand for \"config\"."
        , command = "config"
        , arguments = [] : List Text
        }

      , { alias = "dhall"
        , description = Some "Short hand for \"config --dhall\"."
        , command = "config"
        , arguments = [ "--dhall" ]
        }

      , { alias = "dhall-repl"
        , description = Some "Short hand for \"config --dhall-repl\"."
        , command = "config"
        , arguments = ["--dhall-repl"]
        }
      ]
    : List CommandWrapper.SubcommandAlias
