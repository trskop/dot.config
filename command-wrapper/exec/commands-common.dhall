let CommandWrapper = ../Types.dhall

let commandWrapper = ../library.dhall

let exec = ./library.dhall

let docker = exec.docker

let emptyEnvironment = commandWrapper.command.emptyEnvironment

let List/head-and-tail = commandWrapper.utils.List.head-and-tail

in  -- {{{ Docker -------------------------------------------------------------

    [ commandWrapper.config.exec.namedCommand "docker.prune"
      (docker.prune docker.defaultGlobalOptions emptyEnvironment)
      //  { description = Some "Remove unused images and volumes."
          }

    , commandWrapper.config.exec.namedCommand "docker.shell"
        ( λ(verbosity : CommandWrapper.Verbosity)
        → λ(colourOutput : CommandWrapper.ColourOutput)
        → λ(arguments : List Text)
        → let args = List/head-and-tail Text arguments
          in  Optional/fold Text args.head CommandWrapper.ExecCommand
                ( λ(container : Text)
                → docker.exec container
                  docker.defaultGlobalOptions
                  docker.interactiveExecOptions
                  ( λ(environment : List CommandWrapper.EnvironmentVariable)
                  → λ(verbosity : CommandWrapper.Verbosity)
                  → λ(colourOutput : CommandWrapper.ColourOutput)
                  → λ(arguments : List Text)
                  → let cmd = List/head-and-tail Text arguments
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
                , environment = emptyEnvironment
                , searchPath = True
                , workingDirectory = None Text
                } : CommandWrapper.ExecCommand
        )

    -- }}} Docker -------------------------------------------------------------
    ] : List CommandWrapper.ExecNamedCommand
