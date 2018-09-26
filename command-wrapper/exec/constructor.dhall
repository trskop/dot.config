  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in
    { commands = ./commands.dhall : List CommandWrapper.ExecNamedCommand
    }
