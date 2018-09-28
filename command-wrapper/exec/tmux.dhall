  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    optional = Optional/fold

in
      λ(term : Optional Text)
    → λ(tmuxConfig : Optional Text)
    → λ(workingDirectory : Optional Text)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(arguments : List Text)
    → { command = "tmux"
      , arguments =
          optional Text tmuxConfig (List Text)
            ( λ(value : Text) → ["-f", value]
            )
            ([] : List Text)
          -- TODO: Handle verbosity and pass `-v` option appropriate number of
          -- times.
          # arguments
      , environment =
          environment
          # optional Text term
              (List CommandWrapper.EnvironmentVariable)
              (   λ(value : Text)
                → [ { name = "TERM"
                    , value = value
                    }
                  ]
              )
              ([] : List CommandWrapper.EnvironmentVariable)
      , searchPath = True
--    , workingDirectory = workingDirectory
      }
