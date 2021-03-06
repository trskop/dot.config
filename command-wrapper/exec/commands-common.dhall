let CommandWrapper = ../library.dhall

let Exec = ./library.dhall

let docker = Exec.docker

let Environment/empty = CommandWrapper.Environment.empty

let List/headAndTail = CommandWrapper.List.headAndTail

let Optional/fold =
      https://prelude.dhall-lang.org/v17.0.0/Optional/fold sha256:c5b9d72f6f62bdaa0e196ac1c742cc175cd67a717b880fb8aec1333a5a4132cf

in  -- {{{ Docker -------------------------------------------------------------
    [ CommandWrapper.ExecNamedCommand::{
      , name = "docker.prune"
      , description = Some "Remove unused images and volumes."
      , command = docker.prune docker.GlobalOptions::{=} Environment/empty
      }
    , CommandWrapper.ExecNamedCommand::{
      , name = "docker.shell"
      , command =
          λ(verbosity : CommandWrapper.Verbosity.Type) →
          λ(colourOutput : CommandWrapper.ColourOutput.Type) →
          λ(arguments : List Text) →
            let args = List/headAndTail Text arguments
            
            in  Optional/fold
                  Text
                  args.head
                  CommandWrapper.ExecCommand.Type
                  ( λ(container : Text) →
                      docker.exec
                        container
                        docker.GlobalOptions::{=}
                        docker.ExecOptions.interactive
                        ( λ ( environment
                            : List CommandWrapper.EnvironmentVariable.Type
                            ) →
                          λ(verbosity : CommandWrapper.Verbosity.Type) →
                          λ(colourOutput : CommandWrapper.ColourOutput.Type) →
                          λ(arguments : List Text) →
                            let cmd = List/headAndTail Text arguments
                            
                            in  CommandWrapper.ExecCommand::{
                                , command =
                                    Optional/fold
                                      Text
                                      cmd.head
                                      Text
                                      (λ(exe : Text) → exe)
                                      env:SHELL as Text
                                , arguments = cmd.tail
                                , environment
                                }
                        )
                        Environment/empty
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
