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
      , workingDirectory = None Text
      } : CommandWrapper.ExecCommand
