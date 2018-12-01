let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

let context =
      { home = "${env:HOME as Text}"
      }

let customise = λ(x : CommandWrapper.DefaultConfig) → x

in  commandWrapper.mkDefaultConfig context customise
