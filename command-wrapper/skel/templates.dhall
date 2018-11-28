let
    home = "${env:HOME as Text}"

let
    bashLib = "${home}/.local/trskop/command-wrapper/bash/lib.sh"

let
    haskellSubcommandFileName =
        λ(toolsetName : Text)
      → λ(subcommandName : Text)
      → "${home}/.config/${toolsetName}/toolset/app-${subcommandName}/Main.hs"

in    λ(description : Text)
    → λ(wrapper : Text)
    → λ(subcommand : Text)
    → { haskell =
          { targetFile = Some (haskellSubcommandFileName wrapper subcommand)
          , executable = False
          , template = ./haskell-skel.dhall
          }
      , bash =
          { targetFile = None Text
          , executable = True
          , template = ./bash-skel.dhall bashLib subcommand
          }
      }
