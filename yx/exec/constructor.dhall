let CommandWrapper = ../../command-wrapper/library.dhall

let empty = CommandWrapper.ExecNamedCommand.emptyCommands

in  λ(global : CommandWrapper.ExecConfig.Type) →
        global
      ⫽ { commands =
              global.commands
            # ./commands-common.dhall
            # (./commands.dhall ? empty)
            # (./commands-local.dhall ? empty)
            # (   ~/.local/src/localhost/dot.config/yx/exec/commands-local.dhall
                ? empty
              )
        }
