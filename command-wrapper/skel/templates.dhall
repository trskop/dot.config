let
    home = "${env:HOME as Text}"

in let
    bashLib = "${home}/.local/trskop/command-wrapper/bash/lib.sh"

in let
    haskellSubcommandFileName =
        λ(toolsetName : Text)
      → λ(subcommandName : Text)
      → "${home}/.config/${toolsetName}/toolset/app-${subcommandName}/Main.hs"

in    λ(description : Text)
    → λ(wrapper : Text)
    → λ(subcommand : Text)
    → { haskell =
          { targetFile =
              [haskellSubcommandFileName wrapper subcommand] : Optional Text
          , executable = False
          , template = ./haskell-skel.dhall
          }
      , bash =
          { targetFile = [] : Optional Text
          , executable = True
          , template = ./bash-skel.dhall bashLib
          }
      }
