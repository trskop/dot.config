let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:95094b3603fce0a6374a216904826d5b68168414d117de4fe3786673f38e3c6c

let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:6a3233bf9edea9300226f8842a20152288cd37f4deb53128378352487169a639

let
    null =
      http://prelude.dhall-lang.org/List/null
      sha256:0c3dcbe024ab37387dc2c24854921f586f4c83c3600fbe34ae5233ea2d924783

let
    DockerGlobalOptions : Type = ./DockerGlobalOptions.dhall

let
    DockerExecOptions : Type = ./DockerExecOptions.dhall

let
    DockerRunOptions : Type = ./DockerRunOptions.dhall

let
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

let
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

let
    dockerRunOptions =
        λ(runOptions : DockerRunOptions)
      →   dockerExecOptions
            runOptions.{interactive, allocateTty, detach, user, workingDirectory}
        # ( if runOptions.remove
              then ["--rm"]
              else [] : List Text
          )

let
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

let
    defaultExecOptions =
      { interactive = False
      , allocateTty = False
      , detach = False
      , user = None Text
      , workingDirectory = None Text
      } : DockerExecOptions

let
    execInteractiveCommand =
      { interactive = True
      , allocateTty = True
      , detach = False
      , user = None Text
      , workingDirectory = None Text
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
              , workingDirectory = None Text
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
              , workingDirectory = None Text
              } : CommandWrapper.ExecCommand

    , prune =
          λ(globalOptions : DockerGlobalOptions)
        → λ(environment : List CommandWrapper.EnvironmentVariable)
        → λ(verbosity : CommandWrapper.Verbosity)
        → λ(colourOutput : CommandWrapper.ColourOutput)
        → λ(arguments : List Text)
        → { command = "docker"
          , arguments =
                dockerGlobalOptions globalOptions
              # ["system", "prune", "--volumes"]
              # arguments
          , environment = environment
          , searchPath = True
          , workingDirectory = None Text
          } : CommandWrapper.ExecCommand

    , defaultGlobalOptions =
        { host = [] : List Text
        , logLevel = None Text
        , config = None Text
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
