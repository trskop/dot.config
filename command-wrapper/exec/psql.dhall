let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

let ConnectToDatabase = ./ConnectToDatabase.dhall

in    λ(pgpassFile : Optional Text)
    → λ(psqlrcFile : Optional Text)
    → λ(connect : ConnectToDatabase)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(colourOutput : CommandWrapper.ColourOutput)
    → λ(arguments : List Text)
    → { command = "psql"
      , arguments =
          commandWrapper.verbosity.fold (List Text)
            { Silent = ["--quiet"]
            , Normal = [] : List Text
            , Verbose = [] : List Text
            , Annoying = [] : List Text
            }
            verbosity
          # [ "--host=${connect.hostname}"
            , "--username=${connect.username}"
            , "--dbname=${connect.database}"
            ]
          # Optional/fold Natural connect.port (List Text)
              (λ(port : Natural) → ["--port=${Natural/show port}"])
              ([] : List Text)
          # arguments
      , environment =
          environment
          # Optional/fold Text pgpassFile
              (List CommandWrapper.EnvironmentVariable)
              (   λ(file : Text)
                → [ { name = "PGPASSFILE"
                    , value = file
                    }
                  ]
              )
              ([] : List CommandWrapper.EnvironmentVariable)
          # Optional/fold Text psqlrcFile
              (List CommandWrapper.EnvironmentVariable)
              (   λ(file : Text)
                → [ { name = "PSQLRC"
                    , value = file
                    }
                  ]
              )
              ([] : List CommandWrapper.EnvironmentVariable)
          # commandWrapper.verbosity.fold
              (List CommandWrapper.EnvironmentVariable)
              { Silent =
                  [] : List CommandWrapper.EnvironmentVariable

              , Normal =
                  -- Alternative is to set `VERBOSITY=default`.
                  [] : List CommandWrapper.EnvironmentVariable

              , Verbose =
                  [ { name = "VERBOSITY"
                    , value = "verbose"
                    }
                  ]

              , Annoying =
                  [ { name = "VERBOSITY"
                    , value = "verbose"
                    }
                  ]
              }
              verbosity
      , searchPath = True
      , workingDirectory = None Text
      } : CommandWrapper.ExecCommand
