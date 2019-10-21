let CommandWrapper = ../library.dhall

let Exec = ./library.dhall

let docker = Exec.docker

let emptyEnvironment = CommandWrapper.Command.emptyEnvironment

let List/head-and-tail = CommandWrapper.List.head-and-tail

in  -- {{{ Docker -------------------------------------------------------------

    [ CommandWrapper.ExecNamedCommand.namedCommand "docker.prune"
      (docker.prune docker.defaultGlobalOptions emptyEnvironment)
      //  { description = Some "Remove unused images and volumes."
          }

    , CommandWrapper.ExecNamedCommand.namedCommand "docker.shell"
        ( λ(verbosity : CommandWrapper.Verbosity.Type)
        → λ(colourOutput : CommandWrapper.ColourOutput.Type)
        → λ(arguments : List Text)
        → let args = List/head-and-tail Text arguments
          in  Optional/fold Text args.head CommandWrapper.ExecCommand.Type
                ( λ(container : Text)
                → docker.exec container
                  docker.defaultGlobalOptions
                  docker.interactiveExecOptions
                  ( λ(environment : List CommandWrapper.EnvironmentVariable.Type)
                  → λ(verbosity : CommandWrapper.Verbosity.Type)
                  → λ(colourOutput : CommandWrapper.ColourOutput.Type)
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
                } : CommandWrapper.ExecCommand.Type
        )

    -- }}} Docker -------------------------------------------------------------
    ] : List CommandWrapper.ExecNamedCommand.Type
