let CommandWrapper = ../lib/Types.dhall

in    λ(query : Optional Text)
    → { command = "fzf"
      , arguments =
            [ "--height=40%"
            , "--reverse"
            ]
          # Optional/fold Text query (List Text)
              (λ(query : Text) → ["--query=${query}"])
              ([] : List Text)
      , environment = [] : List CommandWrapper.EnvironmentVariable
      } : CommandWrapper.CommandWithEnvironment
