let CommandWrapper = ./library.dhall

let empty = [] : List CommandWrapper.ExecNamedCommand.Type

in  { commands =
          ./exec/commands-debug.dhall
        # ./exec/commands-common.dhall
        # (./exec/commands.dhall ? empty)
        # (./exec/commands-local.dhall ? empty)
    }
