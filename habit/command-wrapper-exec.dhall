let global = ../command-wrapper/command-wrapper-exec.dhall

in  global //  { commands = global.commands # ./exec/commands.dhall }
