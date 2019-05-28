let CommandWrapper = ../../command-wrapper/Types.dhall

let commandWrapper = ../../command-wrapper/library.dhall

let habitDefaults =
      { aliases = [] : List CommandWrapper.SubcommandAlias
      , helpMessage = ""
      }

let habit = ./aliases.dhall ? habitDefaults

let defaults = commandWrapper.config.toolset.defaults

in  commandWrapper.config.toolset.addSubcommandAliases
      habit.aliases
      habit.helpMessage
        ( defaults
            //  { description = Some "Toolset for work."
                , searchPath =
                    commandWrapper.config.toolset.defaultSearchPath
                    (env:HOME as Text)
                    "habit"
                }
        )
    : CommandWrapper.ToolsetConfig
