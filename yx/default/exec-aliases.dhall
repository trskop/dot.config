let CommandWrapper = ../../command-wrapper/library.dhall

let empty = CommandWrapper.ExecNamedCommand.emptyCommands

let commands =
        ../exec/commands-common.dhall
      # (../exec/commands.dhall ? empty)
      # (../exec/commands-local.dhall ? empty)

in  CommandWrapper.ExecNamedCommand.namedCommandsToAliases commands
