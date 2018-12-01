let CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

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
