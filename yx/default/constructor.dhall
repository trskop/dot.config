let CommandWrapper = ../../command-wrapper/lib/Types.dhall

let commandWrapper = ../../command-wrapper/lib/lib.dhall

let context =
      { home = "${env:HOME as Text}"
      , toolset = "yx"
      }

let yxDefaults =
      { aliases = [] : List CommandWrapper.SubcommandAlias
      , helpMessage = ""
      }

let yx = ./aliases.dhall ? yxDefaults

let customise =
        λ(defaults : CommandWrapper.DefaultConfig)
      → commandWrapper.toolsetConfig.addSubcommandAliases
          yx.aliases
          yx.helpMessage
          defaults
        : CommandWrapper.DefaultConfig

in  commandWrapper.toolsetConfig.mkConfig context customise
