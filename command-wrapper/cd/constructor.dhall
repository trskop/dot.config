let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

in let
    context =
      { home = "${env:HOME as Text}"
      }

in let
    commonDirectories =
      ./common-directories.dhall : List Text

in let
    directories =
      ./directories.dhall ? ([] : List Text)

in let
    customise =
        λ(defaults : CommandWrapper.CdConfig)
      → { directories = defaults.directories # commonDirectories # directories
        , menuTool = ./fzf.dhall
        , shell = "${env:SHELL as Text ? "/bin/bash"}"
        , terminalEmulator = defaults.terminalEmulator
        } : CommandWrapper.CdConfig

in  commandWrapper.mkCdConfig context customise
