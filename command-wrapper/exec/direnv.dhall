let CommandWrapper = ../lib/Types.dhall

in    λ(directory : Optional Text)
    → λ ( mkCommand
          : CommandWrapper.Verbosity
          → CommandWrapper.ColourOutput
          → List Text
          → CommandWrapper.ExecCommand
        )
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(colourOutput : CommandWrapper.ColourOutput)
    → λ(extraArguments : List Text)
    → { command = "direnv"
      , arguments =
          let dir =
                Optional/fold Text directory (List Text) (λ(d : Text) → [d])
                  ([] : List Text)

          let cmd = mkCommand verbosity colourOutput extraArguments

          in  ["exec"] # dir # [cmd.command] # cmd.arguments

      , environment = environment
      , searchPath = True
      , workingDirectory = directory
      } : CommandWrapper.ExecCommand
