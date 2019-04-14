let CommandWrapper = ../../command-wrapper/lib/Types.dhall

let commandWrapper = ../../command-wrapper/lib/lib.dhall

let context =
      { home = "${env:HOME as Text}"
      , toolset = "yx"
      }

let empty =
      { aliases = [] : List CommandWrapper.SubcommandAlias
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
            (./aliases.dhall ? empty)
            (./aliases-common.dhall ? empty)
        )
        ( ./aliases-local.dhall ? empty)

let customise =
        λ(defaults : CommandWrapper.DefaultConfig)
      → commandWrapper.toolsetConfig.addSubcommandAliases
          yx.aliases
          "${helpMessage}${yx.helpMessage}"
          defaults
        : CommandWrapper.DefaultConfig

in  commandWrapper.toolsetConfig.mkConfig context customise
