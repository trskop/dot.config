let CommandWrapper = ../command-wrapper/Types.dhall

let empty = [] : List CommandWrapper.ExecNamedCommand

let global = ../command-wrapper/command-wrapper-exec.dhall

in    global
    //  { commands =
              global.commands
            # ./exec/commands-common.dhall
            # (./exec/commands.dhall ? empty)
            # (./exec/commands-local.dhall ? empty)
        }
