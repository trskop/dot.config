let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in    λ(fileOrUrl : Optional Text)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(colourOutput : CommandWrapper.ColourOutput)
    → λ(arguments : List Text)
    → { command = "xdg-open"
      , arguments =
            Optional/fold Text fileOrUrl (List Text)
              (λ(value : Text) → [value] : List Text)
              ([] : List Text)
          # arguments
      , environment = environment
      , searchPath = True
      , workingDirectory = [] : Optional Text
      } : CommandWrapper.ExecCommand
