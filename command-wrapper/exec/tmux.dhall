  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:08d2673948c732c338f5322ee7c15a3f4b92c27dce731d7c678026eb9309efe6

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
