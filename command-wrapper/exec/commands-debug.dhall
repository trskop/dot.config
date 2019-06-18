let CommandWrapper = ../Types.dhall

let commandWrapper = ../library.dhall

let verbosityToText =
      commandWrapper.verbosity.fold
      Text
      { Silent = "silent"
      , Normal = "normal"
      , Verbose = "verbose"
      , Annoying = "annoying"
      }

in  [   commandWrapper.config.exec.namedCommand "echo"
        (   λ(verbosity : CommandWrapper.Verbosity)
          → λ(colourOutput : CommandWrapper.ColourOutput)
          → λ(arguments : List Text)
          →   { command =
                  "echo"
              , arguments =
                  arguments
              , environment =
                  commandWrapper.command.emptyEnvironment
              , searchPath =
                  True
              , workingDirectory =
                  None Text
              }
            : CommandWrapper.ExecCommand
        )
      // { description = Some "Call echo command (not the shell builtin)." }
    ,   commandWrapper.config.exec.namedCommand "debug"
        (   λ(verbosity : CommandWrapper.Verbosity)
          → λ(colourOutput : CommandWrapper.ColourOutput)
          → λ(arguments : List Text)
          →   { command = "echo"
              , arguments =
                    [ "VERBOSITY=${verbosityToText verbosity}"
                    , "COLOUR_OUTPUT=${commandWrapper.colourOutput.toText
                                       colourOutput}"
                    ]
                  # arguments
              , environment =
                  commandWrapper.command.emptyEnvironment
              , searchPath =
                  True
              , workingDirectory =
                  None Text
              }
            : CommandWrapper.ExecCommand
        )
      //  { description =
              Some
              (     "Call echo command (not the shell builtin); print verbosity"
                ++  " and colour settings allong with arguments."
              )
          }
    ]
