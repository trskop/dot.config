let CommandWrapper = ../library.dhall

let List/map =
      https://prelude.dhall-lang.org/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let dhallAliases =
      List/map
        Text
        CommandWrapper.SubcommandAlias.Type
        (   λ(_ : Text)
          → { alias = "dhall${_}"
            , description = Some "Shorthand for \"config --dhall${_}\"."
            , command = "config"
            , arguments = [ "--dhall${_}" ]
            }
        )
        [ ""
        , "-bash"
        , "-diff"
        , "-exec"
        , "-filter"
        , "-format"
        , "-freeze"
        , "-hash"
        , "-lint"
        , "-repl"
        , "-resolve"
        , "-text"
        ]

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
      ]
    # dhallAliases
