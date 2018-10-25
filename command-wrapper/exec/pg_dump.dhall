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
