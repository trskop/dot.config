let home = "${env:HOME as Text}"

let bashLib = "${home}/.local/src/trskop/command-wrapper/bash/lib.sh"

let config = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let lib = "${home}/.local/lib"

let haskellSubcommandFileName =
        λ(toolsetName : Text)
      → λ(command : Text)
      → "${config}/${toolsetName}/toolset/app-${command}/Main.hs"

let bashSubcommandFileName =
        λ(toolsetName : Text)
      → λ(command : Text)
      → "${lib}/${toolsetName}/${command}"

let dhallConfigFileName =
        λ(toolsetName : Text)
      → λ(command : Text)
      → "${config}/${toolsetName}/${command}.dhall"

in    λ(wrapper : Text)
    → λ(subcommand : Text)
    → λ(command : Text)
    → λ(language : <Bash : {} | Dhall : {} | Haskell : {}>)
    → merge
        { Haskell =
              λ(_ : {})
            → { targetFile = haskellSubcommandFileName wrapper command
              , executable = False
              , template = ./haskell-skel.dhall
              }
        , Bash =
              λ(_ : {})
            → { targetFile = bashSubcommandFileName wrapper command
              , executable = True
              , template = ./bash-skel.dhall bashLib subcommand
              }
        , Dhall =
              λ(_ : {})
            → { targetFile = dhallConfigFileName wrapper command
              , executable = True
              , template = ./dhall-skel.dhall wrapper subcommand
              }
        }
        language
