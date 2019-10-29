let CommandWrapper = ../library.dhall

let verbosityToText =
      CommandWrapper.Verbosity.fold
        Text
        { Silent = "silent"
        , Normal = "normal"
        , Verbose = "verbose"
        , Annoying = "annoying"
        }

in  [ CommandWrapper.ExecNamedCommand::{
      , name = "echo"
      , description = Some "Call echo command (not the shell builtin)."
      , command =
            λ(verbosity : CommandWrapper.Verbosity.Type)
          → λ(colourOutput : CommandWrapper.ColourOutput.Type)
          → λ(arguments : List Text)
          → CommandWrapper.ExecCommand::{
            , command = "echo"
            , arguments = arguments
            }
      }
    , CommandWrapper.ExecNamedCommand::{
      , name = "debug"
      , command =
            λ(verbosity : CommandWrapper.Verbosity.Type)
          → λ(colourOutput : CommandWrapper.ColourOutput.Type)
          → λ(arguments : List Text)
          → CommandWrapper.ExecCommand::{
            , command = "echo"
            , arguments =
                  [ "VERBOSITY=${verbosityToText verbosity}"
                  , "COLOUR_OUTPUT=${CommandWrapper.ColourOutput.toText
                                       colourOutput}"
                  , "COMMAND_WRAPPER_NAME=${env:COMMAND_WRAPPER_NAME as Text}"
                  , "COMMAND_WRAPPER_EXE=${env:COMMAND_WRAPPER_EXE as Text}"
                  , "COMMAND_WRAPPER_VERSION=${env:COMMAND_WRAPPER_VERSION as Text}"
                  , "COMMAND_WRAPPER_SUBCOMMAND=${env:COMMAND_WRAPPER_SUBCOMMAND as Text}"
                  , "COMMAND_WRAPPER_CONFIG=${env:COMMAND_WRAPPER_CONFIG as Text}"
                  ]
                # arguments
            }
      , description =
          Some
            (     "Call echo command (not the shell builtin); print verbosity"
              ++  " and colour settings allong with arguments."
            )
      }
    ]
