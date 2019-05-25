let CommandWrapper = ./Types.dhall

let empty = [] : List CommandWrapper.ExecNamedCommand

in  { commands =
          ./exec/commands-debug.dhall
        # ./exec/commands-common.dhall
        # (./exec/commands.dhall ? empty)
        # (./exec/commands-local.dhall ? empty)
    }
