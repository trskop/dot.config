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

let helpMessage =
      ''

      YX Subcommands:

        env
        jmp
        new
        path
        this
      ''

let home = env:HOME as Text

let xdgDataHome = env:XDG_DATA_HOME as Text ? "${home}/.local/share"

let yx =
      join
        ( join
            (join (./aliases.dhall ? empty) (./aliases-common.dhall ? empty))
            (./aliases-local.dhall ? empty)
        )
        { aliases = ./exec-aliases.dhall, helpMessage = "" }

in    CommandWrapper.ToolsetConfig.addSubcommandAliases
        yx.aliases
        "${helpMessage}${yx.helpMessage}"
        CommandWrapper.ToolsetConfig::{
        , description =
            Some "Y repeate X; set of personalised command line tools."
        , searchPath =
            CommandWrapper.ToolsetConfig.defaultSearchPath home "yx"
        , manPath = [ "${xdgDataHome}/man" ]
        }
    : CommandWrapper.ToolsetConfig.Type
