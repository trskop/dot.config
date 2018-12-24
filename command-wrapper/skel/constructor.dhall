let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

in    λ(wrapper : Text)
    → λ(subcommand : Text)
    → λ(command : Text)
    → { template = ./templates.dhall wrapper subcommand command
      , editAfterwards = True
      } : CommandWrapper.SkelConfig
