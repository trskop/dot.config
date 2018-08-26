  let
    CommandWrapper = ~/Devel/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper = ~/Devel/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    context =
      { home = "${env:HOME as Text}"
      , toolset = "yx"
      }

in let
    yx = ./aliases.dhall

in let
    customise =
        λ(defaults : CommandWrapper.DefaultConfig)
      → commandWrapper.toolsetConfig.addSubcommandAliases
          yx.aliases
          yx.helpMessage
          defaults
        : CommandWrapper.DefaultConfig

in  commandWrapper.toolsetConfig.mkConfig context customise
