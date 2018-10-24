  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    ConnectToDatabase =
      ./ConnectToDatabase.dhall
      sha256:f764a5130c77a2b34136e5c96680c7e95e7870e73c3035f272f58aafd4fc03f1

in
      λ(pgpassFile : Optional Text)
    → λ(psqlrcFile : Optional Text)
    → λ(connect : ConnectToDatabase)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(colourOutput : CommandWrapper.ColourOutput)
    → λ(arguments : List Text)
    → { command = "psql"
      , arguments =
          commandWrapper.verbosity.fold (List Text)
            { Silent = λ(_ : {}) → ["--quiet"]
            , Normal = λ(_ : {}) → [] : List Text
            , Verbose = λ(_ : {}) → [] : List Text
            , Annoying = λ(_ : {}) → [] : List Text
            }
            verbosity
          # [ "--host=${connect.hostname}"
            , "--username=${connect.username}"
            , "--dbname=${connect.database}"
            ]
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
                  λ(_ : {}) → [] : List CommandWrapper.EnvironmentVariable

              , Normal =
                  -- Alternative is to set `VERBOSITY=default`.
                  λ(_ : {}) → [] : List CommandWrapper.EnvironmentVariable

              , Verbose =
                    λ(_ : {})
                  → [ { name = "VERBOSITY"
                      , value = "verbose"
                      }
                    ]

              , Annoying =
                    λ(_ : {})
                  → [ { name = "VERBOSITY"
                      , value = "verbose"
                      }
                    ]
              }
              verbosity
      , searchPath = True
      , workingDirectory = [] : Optional Text
      } : CommandWrapper.ExecCommand
