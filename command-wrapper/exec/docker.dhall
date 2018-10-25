  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:08d2673948c732c338f5322ee7c15a3f4b92c27dce731d7c678026eb9309efe6

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:ca555d6f0c8621b29bb8b7fc7566c2617efa634b5ec1e909cd10346b0ad15faf

in let
    null =
      http://prelude.dhall-lang.org/List/null
      sha256:5f9cb7c6e63e3448509266a3954758f91f32b8540b9115b85dac35e3782b32a5

in let
    DockerGlobalOptions : Type =
      ./DockerGlobalOptions.dhall
      sha256:27154126ac849019c5767d3de6793a23f914be553ba5cc395d494f739a9edc46

in let
    DockerExecOptions : Type =
      ./DockerExecOptions.dhall
      sha256:8de3098208653ed27e9890ccea2a5d70bad199c149eb5b93edcf951d6407ec8a

in let
    DockerRunOptions : Type =
      ./DockerRunOptions.dhall
      sha256:d8e96b50e8ef405d9a1b7a2ca942afac5387c62005f4162b3f4ddc9b01f2fd21

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
