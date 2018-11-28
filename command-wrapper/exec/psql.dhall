let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:95094b3603fce0a6374a216904826d5b68168414d117de4fe3786673f38e3c6c

let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:6a3233bf9edea9300226f8842a20152288cd37f4deb53128378352487169a639

let
    ConnectToDatabase = ./ConnectToDatabase.dhall

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
      , workingDirectory = None Text
      } : CommandWrapper.ExecCommand
