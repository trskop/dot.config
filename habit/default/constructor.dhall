let CommandWrapper = ../../command-wrapper/library.dhall

let emptyAliases = [] : List CommandWrapper.SubcommandAlias.Type

let execAliases = ./exec-aliases.dhall

let userAliases = ./aliases.dhall ? emptyAliases

let hostAliases =
        ~/.local/src/localhost/dot.config/habit/default/aliases.dhall
      ? emptyAliases

let userHelpMessage : Text = ./help-msg.txt as Text ? ""

let hostHelpMessage : Text =
      ~/.local/src/localhost/dot.config/habit/default/help-msg.txt as Text ? ""

let defaults =
      (../config.dhall).add-monorepo-settings
        (../config.dhall).options
        CommandWrapper.ToolsetConfig::{
        , description = Some "Toolset for work."
        , searchPath =
            CommandWrapper.ToolsetConfig.defaultSearchPath
              env:HOME as Text
              "habit"
        , manPath = [ "${env:HOME as Text}/.local/man" ]
        }

in    CommandWrapper.ToolsetConfig.addSubcommandAliases
        (userAliases # hostAliases # execAliases)
        (userHelpMessage ++ hostHelpMessage)
        defaults
    : CommandWrapper.ToolsetConfig.Type
