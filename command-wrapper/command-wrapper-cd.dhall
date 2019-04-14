let CommandWrapper = ./lib/Types.dhall

let commandWrapper = ./lib/lib.dhall

let context =
      { home = "${env:HOME as Text}"
      }

let commonDirectories =
      ./cd/directories-common.dhall : List Text

let empty = [] : List Text

let directories =
      (./cd/directories-local.dhall ? empty) # (./cd/directories.dhall ? empty)

 let
    customise =
        λ(defaults : CommandWrapper.CdConfig)
      → { directories = defaults.directories # commonDirectories # directories
        , menuTool = ./cd/fzf.dhall
        , shell = "${env:SHELL as Text ? "/bin/bash"}"
        , terminalEmulator = defaults.terminalEmulator
        } : CommandWrapper.CdConfig

in  commandWrapper.mkCdConfig context customise
