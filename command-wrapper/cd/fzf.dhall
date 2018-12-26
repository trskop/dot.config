let CommandWrapper = ../lib/Types.dhall

let home = env:HOME as Text

let fzf =
      { command = "fzf"
      , arguments =
          [ "--height", "40%"
          , "--reverse"
          ]
      , environment = [] : List CommandWrapper.EnvironmentVariable
      }

in  fzf : CommandWrapper.CommandWithEnvironment
