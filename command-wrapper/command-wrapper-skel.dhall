let CommandWrapper = ./Types.dhall

let commandWrapper = ./library.dhall

in    λ(wrapper : Text)
    → λ(subcommand : Text)
    → λ(command : Text)
    → { template = ./skel/templates.dhall wrapper subcommand command
      , editAfterwards = True
      } : CommandWrapper.SkelConfig
