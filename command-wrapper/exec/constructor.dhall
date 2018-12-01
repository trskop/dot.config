let CommandWrapper = ../lib/Types.dhall

in
    { commands =
        ./commands.dhall ? ([] : List CommandWrapper.ExecNamedCommand)
    }
