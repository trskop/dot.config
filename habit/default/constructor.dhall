  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    context =
      { home = "${env:HOME as Text}"
      , toolset = "habit"
      }

in let
    habitDefaults =
      { aliases = [] : List CommandWrapper.SubcommandAlias
      , helpMessage = ""
      }

in let
    habit = ./aliases.dhall ? habitDefaults

in let
    customise =
        λ(defaults : CommandWrapper.DefaultConfig)
      → commandWrapper.toolsetConfig.addSubcommandAliases
          habit.aliases
          habit.helpMessage
          defaults
        : CommandWrapper.DefaultConfig

in  commandWrapper.toolsetConfig.mkConfig context customise
