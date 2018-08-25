  let
    CommandWrapper = ~/Devel/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper = ~/Devel/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    context =
      { home = "${env:HOME as Text}"
      }

in let
    customise =
        λ(defaults : CommandWrapper.CdConfig)
      → { directories = defaults.directories # (./directories.dhall : List Text)
        , menuTool = ./fzf.dhall
        , shell = "${env:SHELL as Text ? "/bin/bash"}"
        , terminalEmulator = defaults.terminalEmulator
        } : CommandWrapper.CdConfig

in  commandWrapper.mkCdConfig context customise
