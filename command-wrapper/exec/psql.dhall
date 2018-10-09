  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    optional = Optional/fold

in let
    ConnectToDatabase = ./ConnectToDatabase.dhall

in
      λ(pgpassFile : Optional Text)
    → λ(psqlrcFile : Optional Text)
    → λ(connect : ConnectToDatabase)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(arguments : List Text)
    → { command = "psql"
      , arguments =
          [ "--host=${connect.hostname}"
          , "--username=${connect.username}"
          , "--dbname=${connect.database}"
          ] # arguments
          -- TODO: Handle case when verbosity is set to `Silent` by passing
          -- `-q`/`--quiet` option to `psql`.
      , environment =
          environment
          # optional Text pgpassFile
              (List CommandWrapper.EnvironmentVariable)
              (   λ(file : Text)
                → [ { name = "PGPASSFILE"
                    , value = file
                    }
                  ]
              )
              ([] : List CommandWrapper.EnvironmentVariable)
          # optional Text psqlrcFile
              (List CommandWrapper.EnvironmentVariable)
              (   λ(file : Text)
                → [ { name = "PSQLRC"
                    , value = file
                    }
                  ]
              )
              ([] : List CommandWrapper.EnvironmentVariable)
          -- TODO: Handle case when verbosity is set to ohter values than
          -- `Silent` by defining `VERBOSITY` environment variable.
      , searchPath = True
      , workingDirectory = [] : Optional Text
      }
