let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:08d2673948c732c338f5322ee7c15a3f4b92c27dce731d7c678026eb9309efe6

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
