let CommandWrapper = ../lib/Types.dhall

in
    { commands =
          ./debug-commands.dhall
        # (./commands.dhall ? ([] : List CommandWrapper.ExecNamedCommand))
    }
