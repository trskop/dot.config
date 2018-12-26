let CommandWrapper = ../../command-wrapper/lib/Types.dhall

let commandWrapper = ../../command-wrapper/lib/lib.dhall

let context =
      { home = "${env:HOME as Text}"
      , toolset = "habit"
      }

let habitDefaults =
      { aliases = [] : List CommandWrapper.SubcommandAlias
      , helpMessage = ""
      }

let habit = ./aliases.dhall ? habitDefaults

let customise =
        λ(defaults : CommandWrapper.DefaultConfig)
      → commandWrapper.toolsetConfig.addSubcommandAliases
          habit.aliases
          habit.helpMessage
          defaults
        : CommandWrapper.DefaultConfig

in  commandWrapper.toolsetConfig.mkConfig context customise
