let CommandWrapper = ../Types.dhall

let commandWrapper = ../library.dhall

let verbosityToText = commandWrapper.verbosity.fold Text
      { Silent = "silent"
      , Normal = "normal"
      , Verbose = "verbose"
      , Annoying = "annoying"
      }

in  -- Debugging commands:
    [ { name = "echo"
      , description = Some "Call echo command (not the shell builtin)."
      , command =
            λ(verbosity : CommandWrapper.Verbosity)
          → λ(colourOutput : CommandWrapper.ColourOutput)
          → λ(arguments : List Text)
          → { command = "echo"
            , arguments = arguments
            , environment = [] : List CommandWrapper.EnvironmentVariable
            , searchPath = True
            , workingDirectory = None Text
            } : CommandWrapper.ExecCommand

      , completion =
          None
          (   CommandWrapper.Shell
            → Natural
            → List Text
            → CommandWrapper.ExecCommand
          )

      } : CommandWrapper.ExecNamedCommand

    , { name = "debug"
      , description =
          Some
          (     "Call echo command (not the shell builtin); print verbosity"
            ++  " and colour settings allong with arguments."
          )
      , command =
            λ(verbosity : CommandWrapper.Verbosity)
          → λ(colourOutput : CommandWrapper.ColourOutput)
          → λ(arguments : List Text)
          → { command = "echo"
            , arguments =
                [ "VERBOSITY=${verbosityToText verbosity}"
                , "COLOUR_OUTPUT=${commandWrapper.colourOutput.toText colourOutput}"
                ]
                # arguments
            , environment = [] : List CommandWrapper.EnvironmentVariable
            , searchPath = True
            , workingDirectory = None Text
            } : CommandWrapper.ExecCommand

      , completion =
          None
          (   CommandWrapper.Shell
            → Natural
            → List Text
            → CommandWrapper.ExecCommand
          )
      } : CommandWrapper.ExecNamedCommand
    ]
