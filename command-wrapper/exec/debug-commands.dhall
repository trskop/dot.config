let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

let verbosityToText = commandWrapper.verbosity.fold Text
      { Silent = λ(_ : {}) → "silent"
      , Normal = λ(_ : {}) → "normal"
      , Verbose = λ(_ : {}) → "verbose"
      , Annoying = λ(_ : {}) → "annoying"
      }

in  -- Debugging commands:
    [ { name = "echo"
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
      } : CommandWrapper.ExecNamedCommand

    , { name = "debug"
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
      } : CommandWrapper.ExecNamedCommand
    ]
