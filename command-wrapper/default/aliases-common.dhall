let CommandWrapper = ../Types.dhall

in    [ { alias = "h"
        , description = Some "Shorthand for \"help\"."
        , arguments = [] : List Text
        , command = "help"
        }

      , { alias = "man"
        , description = Some "Shorthand for \"help --man\"."
        , command = "help"
        , arguments = [ "--man" ]
        }

      , { alias = "cfg"
        , description = Some "Shorthand for \"config\"."
        , command = "config"
        , arguments = [] : List Text
        }

      , { alias = "dhall"
        , description = Some "Shorthand for \"config --dhall\"."
        , command = "config"
        , arguments = [ "--dhall" ]
        }

      , { alias = "dhall-repl"
        , description = Some "Shorthand for \"config --dhall-repl\"."
        , command = "config"
        , arguments = ["--dhall-repl"]
        }
      ]
    : List CommandWrapper.SubcommandAlias
