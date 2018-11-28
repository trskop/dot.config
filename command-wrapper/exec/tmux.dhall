let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:95094b3603fce0a6374a216904826d5b68168414d117de4fe3786673f38e3c6c

in
      λ(term : Optional Text)
    → λ(tmuxConfig : Optional Text)
    → λ(workingDirectory : Optional Text)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(colourOutput : CommandWrapper.ColourOutput)
    → λ(arguments : List Text)
    → { command = "tmux"
      , arguments =
          Optional/fold Text tmuxConfig (List Text)
            ( λ(value : Text) → ["-f", value]
            )
            ([] : List Text)
          -- TODO: Handle verbosity and pass `-v` option appropriate number of
          -- times.
          # arguments
      , environment =
          environment
          # Optional/fold Text term
              (List CommandWrapper.EnvironmentVariable)
              (   λ(value : Text)
                → [ { name = "TERM"
                    , value = value
                    }
                  ]
              )
              ([] : List CommandWrapper.EnvironmentVariable)
      , searchPath = True
      , workingDirectory = workingDirectory
      } : CommandWrapper.ExecCommand
