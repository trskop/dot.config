  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:08d2673948c732c338f5322ee7c15a3f4b92c27dce731d7c678026eb9309efe6

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:ca555d6f0c8621b29bb8b7fc7566c2617efa634b5ec1e909cd10346b0ad15faf

in let
    ConnectToDatabase =
      ./ConnectToDatabase.dhall
      sha256:4b2567ac9b4cb64394df716190a7d8e28d2d1e5fefaeff038108182a9a5a8550

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
