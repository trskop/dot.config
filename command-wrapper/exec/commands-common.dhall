let CommandWrapper = ../lib/Types.dhall

let docker = ./docker.dhall

let noExtraEnvironment = [] : List CommandWrapper.EnvironmentVariable

let headAndTail = ./head-and-tail.dhall

in  -- {{{ Docker -------------------------------------------------------------

    [ { name = "docker.prune"
      , command = docker.prune docker.defaultGlobalOptions noExtraEnvironment
      }

    , { name = "docker.shell"
      , command =
          λ(verbosity : CommandWrapper.Verbosity)
        → λ(colourOutput : CommandWrapper.ColourOutput)
        → λ(arguments : List Text)
        → let args = headAndTail Text arguments
          in  Optional/fold Text args.head CommandWrapper.ExecCommand
                ( λ(container : Text)
                → docker.exec container
                  docker.defaultGlobalOptions
                  docker.execInteractiveCommand
                  ( λ(environment : List CommandWrapper.EnvironmentVariable)
                  → λ(verbosity : CommandWrapper.Verbosity)
                  → λ(colourOutput : CommandWrapper.ColourOutput)
                  → λ(arguments : List Text)
                  → let cmd = headAndTail Text arguments
                    in  { command =
                           Optional/fold Text cmd.head Text
                             (λ(exe : Text) → exe)
                             (env:SHELL as Text)
                        , arguments = cmd.tail
                        , environment = environment
                        , searchPath = True
                        , workingDirectory = None Text
                        }
                  )
                  noExtraEnvironment
                  verbosity
                  colourOutput
                  args.tail
                )
                { command = "echo"
                , arguments = ["ERROR: Missing arugment: container name"]
                , environment = [] : List CommandWrapper.EnvironmentVariable
                , searchPath = True
                , workingDirectory = None Text
                } : CommandWrapper.ExecCommand
      }

    -- }}} Docker -------------------------------------------------------------
    ] : List CommandWrapper.ExecNamedCommand
