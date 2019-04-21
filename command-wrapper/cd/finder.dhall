let CommandWrapper = ../lib/Types.dhall

in  { fzf =
          λ(query : Optional Text)
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

    , fzy =
          λ(query : Optional Text)
        → { command = "fzy"
          , arguments =
              Optional/fold Text query (List Text)
                (λ(query : Text) → ["--query=${query}"])
                ([] : List Text)
          , environment = [] : List CommandWrapper.EnvironmentVariable
          } : CommandWrapper.CommandWithEnvironment
    }
