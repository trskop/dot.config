let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

let ConnectToDatabase = ./ConnectToDatabase.dhall

in    λ(connect : ConnectToDatabase)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(_ : CommandWrapper.ColourOutput)
    → λ(arguments : List Text)
    → { command = "pg_dump"
      , arguments =
          commandWrapper.verbosity.fold (List Text)
            { Silent = [] : List Text
            , Normal = [] : List Text
            , Verbose = ["--verbose"]
            , Annoying = ["--verbose"]
            }
            verbosity
          # [ "--host=${connect.hostname}"
            , "--username=${connect.username}"
            , "--dbname=${connect.database}"
            ]
          # arguments
      , environment = environment
      , searchPath = True
      , workingDirectory = None Text
      } : CommandWrapper.ExecCommand
