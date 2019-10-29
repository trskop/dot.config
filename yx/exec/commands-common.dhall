let CommandWrapper = ../../command-wrapper/library.dhall

let Exec = ../../command-wrapper/exec/library.dhall

let emptyArguments = CommandWrapper.Command.emptyArguments

let emptyEnvironment = CommandWrapper.Command.emptyEnvironment

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

in  [ CommandWrapper.ExecNamedCommand::{
      , name = "direnv"
      , description =
          Some "Just invoke 'direnv', but with command line completion."
      , command =
          Exec.direnv.command (None Text) emptyArguments emptyEnvironment
      , completion =
          Some (Exec.direnv.completion toolset (None Text) emptyArguments)
      }
    , CommandWrapper.ExecNamedCommand::{
      , name = "jq"
      , description = Some "Just invoke 'jq', but with command line completion."
      , command = Exec.jq.command emptyArguments
      , completion =
          Some (Exec.jq.completion toolset (None Text) emptyArguments)
      }
    ]
