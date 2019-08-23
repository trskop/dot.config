let CommandWrapper = ../../command-wrapper/Types.dhall

let commandWrapper = ../../command-wrapper/library.dhall

let habitDefaults =
      { aliases = [] : List CommandWrapper.SubcommandAlias
      , helpMessage = ""
      }

let execAliases = ./exec-aliases.dhall

let habit = ./aliases.dhall ? habitDefaults

let defaults = commandWrapper.config.toolset.defaults

let extraSearchPath = (../config.dhall).searchPath ? ([] : List Text)

in    commandWrapper.config.toolset.addSubcommandAliases
      (habit.aliases # execAliases)
      habit.helpMessage
      (   defaults
        //  { description = Some "Toolset for work."
            , searchPath =
                  extraSearchPath
                # commandWrapper.config.toolset.defaultSearchPath
                  env:HOME as Text
                  "habit"
            }
      )
    : CommandWrapper.ToolsetConfig
