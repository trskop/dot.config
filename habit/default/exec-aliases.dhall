let CommandWrapper = ~/.config/command-wrapper/library.dhall

let execConfig = ../command-wrapper-exec.dhall

in CommandWrapper.ExecNamedCommand.namedCommandsToAliases execConfig.commands
