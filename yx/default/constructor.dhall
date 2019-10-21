let CommandWrapper = ../../command-wrapper/library.dhall

let empty =
      { aliases = CommandWrapper.ToolsetConfig.emptyAliases
      , helpMessage = ""
      }

let join =
        λ ( a
          : { aliases : List CommandWrapper.SubcommandAlias.Type
            , helpMessage : Text
            }
          )
      → λ ( b
          : { aliases : List CommandWrapper.SubcommandAlias.Type
            , helpMessage : Text
            }
          )
      → { aliases = a.aliases # b.aliases
        , helpMessage = "${a.helpMessage}${b.helpMessage}"
        }

let helpMessage = ''

YX Subcommands:

  env
  jmp
  new
  path
  this
''

let yx =
      join
        ( join
            (join (./aliases.dhall ? empty) (./aliases-common.dhall ? empty))
            (./aliases-local.dhall ? empty)
        )
        { aliases = ./exec-aliases.dhall, helpMessage = "" }

let defaults =
      CommandWrapper.ToolsetConfig::{
      , description =
          Some "Y repeate X; set of personalised command line tools."
      }

in    CommandWrapper.ToolsetConfig.addSubcommandAliases
        yx.aliases
        "${helpMessage}${yx.helpMessage}"
        (     defaults
          //  { searchPath =
                    defaults.searchPath
                  # CommandWrapper.ToolsetConfig.defaultSearchPath
                      env:HOME as Text
                      "yx"
              , manPath =
                  defaults.manPath # [ "${env:HOME as Text}/.local/man" ]
              }
        )
    : CommandWrapper.ToolsetConfig.Type
