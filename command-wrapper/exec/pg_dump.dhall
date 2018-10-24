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
      λ(connect : ConnectToDatabase)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(_ : CommandWrapper.ColourOutput)
    → λ(arguments : List Text)
    → { command = "pg_dump"
      , arguments =
          commandWrapper.verbosity.fold (List Text)
            { Silent = λ(_ : {}) → [] : List Text
            , Normal = λ(_ : {}) → [] : List Text
            , Verbose = λ(_ : {}) → ["--verbose"]
            , Annoying = λ(_ : {}) → ["--verbose"]
            }
            verbosity
          # [ "--host=${connect.hostname}"
            , "--username=${connect.username}"
            , "--dbname=${connect.database}"
            ]
          # arguments
      , environment = environment
      , searchPath = True
      , workingDirectory = [] : Optional Text
      } : CommandWrapper.ExecCommand
