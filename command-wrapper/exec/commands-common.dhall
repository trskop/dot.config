let CommandWrapper = ../library.dhall

let Exec = ./library.dhall

let docker = Exec.docker

let emptyEnvironment = CommandWrapper.Command.emptyEnvironment

let List/head-and-tail = CommandWrapper.List.head-and-tail

in  -- {{{ Docker -------------------------------------------------------------
    [ CommandWrapper.ExecNamedCommand::{
      , name = "docker.prune"
      , description = Some "Remove unused images and volumes."
      , command = docker.prune docker.defaultGlobalOptions emptyEnvironment
      }
    , CommandWrapper.ExecNamedCommand::{
      , name = "docker.shell"
      , command =
            λ(verbosity : CommandWrapper.Verbosity.Type)
          → λ(colourOutput : CommandWrapper.ColourOutput.Type)
          → λ(arguments : List Text)
          → let args = List/head-and-tail Text arguments

            in  Optional/fold
                  Text
                  args.head
                  CommandWrapper.ExecCommand.Type
                  (   λ(container : Text)
                    → docker.exec
                        container
                        docker.defaultGlobalOptions
                        docker.interactiveExecOptions
                        (   λ ( environment
                              : List CommandWrapper.EnvironmentVariable.Type
                              )
                          → λ(verbosity : CommandWrapper.Verbosity.Type)
                          → λ(colourOutput : CommandWrapper.ColourOutput.Type)
                          → λ(arguments : List Text)
                          → let cmd = List/head-and-tail Text arguments

                            in  CommandWrapper.ExecCommand::{
                                , command =
                                    Optional/fold
                                      Text
                                      cmd.head
                                      Text
                                      (λ(exe : Text) → exe)
                                      env:SHELL as Text
                                , arguments = cmd.tail
                                , environment = environment
                                }
                        )
                        emptyEnvironment
                        verbosity
                        colourOutput
                        args.tail
                  )
                  CommandWrapper.ExecCommand::{
                  , command = "echo"
                  , arguments = [ "ERROR: Missing arugment: container name" ]
                  }
      }
    -- }}} Docker -------------------------------------------------------------
    ]
