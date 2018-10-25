  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:ca555d6f0c8621b29bb8b7fc7566c2617efa634b5ec1e909cd10346b0ad15faf

in let
    context =
      { home = "${env:HOME as Text}"
      }

in let
    directories =
      ./directories.dhall ? ([] : List Text)

in let
    customise =
        λ(defaults : CommandWrapper.CdConfig)
      → { directories = defaults.directories # directories
        , menuTool = ./fzf.dhall
        , shell = "${env:SHELL as Text ? "/bin/bash"}"
        , terminalEmulator = defaults.terminalEmulator
        } : CommandWrapper.CdConfig

in  commandWrapper.mkCdConfig context customise
