  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    null =
      http://prelude.dhall-lang.org/List/null
      sha256:c199cbd6960227e70057ee9b791d506b776e0ac98dadde8b2955d5154ff96d21

in let
    ConnectToDatabase : Type =
      ./ConnectToDatabase.dhall
      sha256:f764a5130c77a2b34136e5c96680c7e95e7870e73c3035f272f58aafd4fc03f1

in let
    DockerGlobalOptions : Type =
      ./DockerGlobalOptions.dhall
      sha256:5b0874eb31f38d2cd7b164703b9047c81479c6df541a4d31c2ba28be6bc6f04a

in let
    DockerExecOptions : Type =
      ./DockerExecOptions.dhall
      sha256:4c5fcb96707e8d3ce1395151e60ec7191310c733578578211ec73a4da1d9a98b

in let
    DockerRunOptions : Type =
      ./DockerRunOptions.dhall
--    sha256:68489955ac37ddf3fa0c34aa5251ecae8ee7fa960a634d2c7e6e6df47aeca255

in let
    dockerGlobalOptions =
        λ(globalOptions : DockerGlobalOptions)
      →   List/fold Text globalOptions.host (List Text)
            (   λ(host : Text)
              → λ(options : List Text)
              → options # ["--host", host]
            )
            ([] : List Text)
        # Optional/fold Text globalOptions.logLevel (List Text)
            (λ(config : Text) → ["--config", config])
            ([] : List Text)
        # Optional/fold Text globalOptions.config (List Text)
            (λ(config : Text) → ["--config", config])
            ([] : List Text)

in let
    dockerExecOptions =
        λ(execOptions : DockerExecOptions)
      →   ( if execOptions.interactive
              then ["--interactive"]
              else [] : List Text
          )
        # ( if execOptions.allocateTty
              then ["--tty"]
              else [] : List Text
          )
        # ( if execOptions.detach
              then ["--detach"]
              else [] : List Text
          )
        # Optional/fold Text execOptions.user (List Text)
            (λ(user : Text) → ["--user", user])
            ([] : List Text)
        # Optional/fold Text execOptions.workingDirectory (List Text)
            (λ(dir : Text) → ["--workdir", dir])
            ([] : List Text)

in let
    dockerRunOptions =
        λ(runOptions : DockerRunOptions)
      →   dockerExecOptions
            runOptions.{interactive, allocateTty, detach, user, workingDirectory}
        # ( if runOptions.remove
              then ["--rm"]
              else [] : List Text
          )

in let
    dockerEnvOptions =
        λ(environment : List CommandWrapper.EnvironmentVariable)
      → List/fold
          CommandWrapper.EnvironmentVariable
          environment
          (List Text)
          ( λ(var : CommandWrapper.EnvironmentVariable)
          → λ(accum : List Text)
          → ["--env", "${var.name}=${var.value}"] # accum
          )
          ([] : List Text)

in let
    defaultExecOptions =
      { interactive = False
      , allocateTty = False
      , detach = False
      , user = [] : Optional Text
      , workingDirectory = [] : Optional Text
      } : DockerExecOptions

in let
    execInteractiveCommand =
      { interactive = True
      , allocateTty = True
      , detach = False
      , user = [] : Optional Text
      , workingDirectory = [] : Optional Text
      } : DockerExecOptions

in
    -- Transform a command into:
    --
    -- ```
    -- docker GLOBAL_OPTIONS exec EXEC_OPTIONS CONTAINER COMMAND
    -- ```
    { exec =
          λ(container : Text)
        → λ(globalOptions : DockerGlobalOptions)
        → λ(execOptions : DockerExecOptions)
        → λ ( mkCommand
              : ∀(environment : List CommandWrapper.EnvironmentVariable)
              → ∀(verbosity : CommandWrapper.Verbosity)
              → ∀(colourOutput : CommandWrapper.ColourOutput)
              → ∀(arguments : List Text)
              → CommandWrapper.ExecCommand
            )
        → λ(environment : List CommandWrapper.EnvironmentVariable)
        → λ(verbosity : CommandWrapper.Verbosity)
        → λ(colourOutput : CommandWrapper.ColourOutput)
        → λ(arguments : List Text)
        → let command = mkCommand environment verbosity colourOutput arguments
          in  { command = "docker"
              , arguments =
                    dockerGlobalOptions globalOptions
                  # ["exec"]
                  # dockerExecOptions execOptions
                  # [container]
                  # dockerEnvOptions command.environment
                  # [command.command]
                  # command.arguments
              , environment = [] : List CommandWrapper.EnvironmentVariable
              , searchPath = True
              , workingDirectory = [] : Optional Text
              } : CommandWrapper.ExecCommand

    -- Transform a command into:
    --
    -- ```
    -- docker GLOBAL_OPTIONS run RUN_OPTIONS CONTAINER COMMAND
    -- ```
    , run =
          λ(container : Text)
        → λ(globalOptions : DockerGlobalOptions)
        → λ(runOptions : DockerRunOptions)
        → λ ( mkCommand
              : ∀(environment : List CommandWrapper.EnvironmentVariable)
              → ∀(verbosity : CommandWrapper.Verbosity)
              → ∀(colourOutput : CommandWrapper.ColourOutput)
              → ∀(arguments : List Text)
              → CommandWrapper.ExecCommand
            )
        → λ(environment : List CommandWrapper.EnvironmentVariable)
        → λ(verbosity : CommandWrapper.Verbosity)
        → λ(colourOutput : CommandWrapper.ColourOutput)
        → λ(arguments : List Text)
        → let command = mkCommand environment verbosity colourOutput arguments
          in  { command = "docker"
              , arguments =
                    dockerGlobalOptions globalOptions
                  # ["run"]
                  # dockerRunOptions runOptions
                  # [container]
                  # dockerEnvOptions command.environment
                  # [command.command]
                  # command.arguments
              , environment = [] : List CommandWrapper.EnvironmentVariable
              , searchPath = True
              , workingDirectory = [] : Optional Text
              } : CommandWrapper.ExecCommand

    , defaultGlobalOptions =
        { host = [] : List Text
        , logLevel = [] : Optional Text
        , config = [] : Optional Text
        } : DockerGlobalOptions

    , defaultExecOptions = defaultExecOptions

    , defaultRunOptions =
        ( defaultExecOptions
        /\  { remove = False
            }
        ) : DockerRunOptions

    , execInteractiveCommand = execInteractiveCommand

    , runInteractiveCommand =
        ( execInteractiveCommand
        /\  { remove = False
            }
        ) : DockerRunOptions

    , runEphemeral =
          λ(runOptions : DockerRunOptions)
        → ( runOptions.{interactive, allocateTty, detach, user, workingDirectory}
          /\  { remove = True
              }
          ) : DockerRunOptions
    }
