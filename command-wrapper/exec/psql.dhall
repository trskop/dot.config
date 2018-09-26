  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    optional = Optional/fold

in
      λ(pgpassFile : Optional Text)
    → λ(hostname : Text)
    → λ(username : Text)
    → λ(database : Text)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(arguments : List Text)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → { command = "psql"
      , arguments =
          ["-h", hostname, "-U", username, database] # arguments
          -- TODO: Handle case when verbosity is set to `Silent` by passing
          -- `-q`/`--quiet` option to `psql`.
      , environment =
          optional Text pgpassFile
            (List CommandWrapper.EnvironmentVariable)
            (   λ(file : Text)
              → [ { name = "PGPASSFILE"
                  , value = file
                  }
                ]
            )
            ([] : List CommandWrapper.EnvironmentVariable)
          -- TODO: Handle case when verbosity is set to ohter values than
          -- `Silent` by defining `VERBOSITY` environment variable.
          # environment
      , searchPath = True
      }
