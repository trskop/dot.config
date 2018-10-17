  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    context =
      { home = "${env:HOME as Text}"
      , toolset = "yx"
      }

in let
    yxDefaults =
      { aliases = [] : List CommandWrapper.SubcommandAlias
      , helpMessage = ""
      }

in let
    yx = ./aliases.dhall ? yxDefaults

in let
    customise =
        λ(defaults : CommandWrapper.DefaultConfig)
      → commandWrapper.toolsetConfig.addSubcommandAliases
          yx.aliases
          yx.helpMessage
          defaults
        : CommandWrapper.DefaultConfig

in  commandWrapper.toolsetConfig.mkConfig context customise
