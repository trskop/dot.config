let CommandWrapper = ../../command-wrapper/Types.dhall

let commandWrapper = ../../command-wrapper/library.dhall

let empty =
      { aliases = commandWrapper.config.toolset.emptyAliases
      , helpMessage = ""
      }

let join =
        λ(a : {aliases : List CommandWrapper.SubcommandAlias, helpMessage : Text})
      → λ(b : {aliases : List CommandWrapper.SubcommandAlias, helpMessage : Text})
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
        ( join
          (./aliases.dhall ? empty)
          (./aliases-common.dhall ? empty)
        )
        (./aliases-local.dhall ? empty)
      )
      { aliases = ./exec-aliases.dhall
      , helpMessage = ""
      }

let defaults = commandWrapper.config.toolset.defaults

in    commandWrapper.config.toolset.addSubcommandAliases
        yx.aliases
        "${helpMessage}${yx.helpMessage}"
        ( defaults
            //  { description =
                    Some "Y repeate X; set of personalised command line tools."

                , searchPath =
                    commandWrapper.config.toolset.defaultSearchPath
                      (env:HOME as Text)
                      "yx"
                }
        )
    : CommandWrapper.ToolsetConfig
