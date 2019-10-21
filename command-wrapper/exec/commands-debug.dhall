let CommandWrapper = ../library.dhall

let verbosityToText =
      CommandWrapper.Verbosity.fold
        Text
        { Silent = "silent"
        , Normal = "normal"
        , Verbose = "verbose"
        , Annoying = "annoying"
        }

in  [     CommandWrapper.ExecNamedCommand.namedCommand
            "echo"
            (   λ(verbosity : CommandWrapper.Verbosity.Type)
              → λ(colourOutput : CommandWrapper.ColourOutput.Type)
              → λ(arguments : List Text)
              →   { command = "echo"
                  , arguments = arguments
                  , environment = CommandWrapper.Command.emptyEnvironment
                  , searchPath = True
                  , workingDirectory = None Text
                  }
                : CommandWrapper.ExecCommand.Type
            )
      //  { description = Some "Call echo command (not the shell builtin)." }
    ,     CommandWrapper.ExecNamedCommand.namedCommand
            "debug"
            (   λ(verbosity : CommandWrapper.Verbosity.Type)
              → λ(colourOutput : CommandWrapper.ColourOutput.Type)
              → λ(arguments : List Text)
              →   { command = "echo"
                  , arguments =
                        [ "VERBOSITY=${verbosityToText verbosity}"
                        , "COLOUR_OUTPUT=${CommandWrapper.ColourOutput.toText
                                             colourOutput}"
                        ]
                      # arguments
                  , environment = CommandWrapper.Command.emptyEnvironment
                  , searchPath = True
                  , workingDirectory = None Text
                  }
                : CommandWrapper.ExecCommand.Type
            )
      //  { description =
              Some
                (     "Call echo command (not the shell builtin); print"
                  ++  " verbosity and colour settings allong with arguments."
                )
          }
    ]
