let CommandWrapper = ../Types.dhall

let commandWrapper = ../library.dhall

let docker = commandWrapper.config.exec.docker

let emptyEnvironment = commandWrapper.command.emptyEnvironment

let headAndTail = commandWrapper.utils.List.head-and-tail

in  -- {{{ Docker -------------------------------------------------------------

    [ { name = "docker.prune"
      , description = Some "Remove unused images and volumes."
      , command = docker.prune docker.defaultGlobalOptions emptyEnvironment
      , completion =
          None
          (   CommandWrapper.Shell
            → Natural
            → List Text
            → CommandWrapper.ExecCommand
          )
      }

    , { name = "docker.shell"
      , description = None Text
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
                  emptyEnvironment
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

      , completion =
          None
          (   CommandWrapper.Shell
            → Natural
            → List Text
            → CommandWrapper.ExecCommand
          )
      }

    -- }}} Docker -------------------------------------------------------------
    ] : List CommandWrapper.ExecNamedCommand
