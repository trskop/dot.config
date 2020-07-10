let CommandWrapper = ../../command-wrapper/library.dhall

let constructor = ../exec/constructor.dhall

let commands = (constructor CommandWrapper.ExecConfig::{=}).commands

in  CommandWrapper.ExecNamedCommand.namedCommandsToAliases commands
