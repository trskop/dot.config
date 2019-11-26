let CommandWrapper = ../command-wrapper/library.dhall

let empty = CommandWrapper.ExecNamedCommand.emptyCommands

let global = ../command-wrapper/command-wrapper-exec.dhall

in    global
    ⫽ { commands =
            global.commands
          # ./exec/commands-common.dhall
          # (./exec/commands.dhall ? empty)
          # (./exec/commands-local.dhall ? empty)
          # (   ~/.local/src/localhost/this/dot.config/yx/exec/commands-local.dhall
              ? emptyDirectories
            )
      }
