  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    context =
      { home = "${env:HOME as Text}"
      }

in let
    customise = λ(x : CommandWrapper.DefaultConfig) → x

in  commandWrapper.mkDefaultConfig context customise
