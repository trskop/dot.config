  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    context =
      { home = "${env:HOME as Text}"
      }

in  commandWrapper.mkSkelConfig context ./templates.dhall
