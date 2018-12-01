let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

let context =
      { home = "${env:HOME as Text}"
      }

in  commandWrapper.mkSkelConfig context ./templates.dhall
