let CommandWrapper = ../lib/Types.dhall

in    λ(query : Optional Text)
    → { command = "fzy"
      , arguments =
          Optional/fold Text query (List Text)
            (λ(query : Text) → ["--query=${query}"])
            ([] : List Text)
      , environment = [] : List CommandWrapper.EnvironmentVariable
      } : CommandWrapper.CommandWithEnvironment
