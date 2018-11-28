let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:95094b3603fce0a6374a216904826d5b68168414d117de4fe3786673f38e3c6c

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
      , workingDirectory = None Text
      } : CommandWrapper.ExecCommand
