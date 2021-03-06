let CommandWrapper = ./library.dhall

let empty = [] : List CommandWrapper.ExecNamedCommand.Type

in  CommandWrapper.ExecConfig::{
    , commands =
          ./exec/commands-debug.dhall
        # ./exec/commands-common.dhall
        # (./exec/commands.dhall ? empty)
        # (./exec/commands-local.dhall ? empty)
        # (   ~/.local/src/localhost/dot.config/command-wrapper/exec/commands-local.dhall
            ? empty
          )
    }
