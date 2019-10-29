let CommandWrapper = ../../command-wrapper/library.dhall

let habitDefaults =
      { aliases = [] : List CommandWrapper.SubcommandAlias.Type
      , helpMessage = ""
      }

let execAliases = ./exec-aliases.dhall

let habit = ./aliases.dhall ? habitDefaults

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
        (habit.aliases # execAliases)
        habit.helpMessage
        defaults
    : CommandWrapper.ToolsetConfig.Type
