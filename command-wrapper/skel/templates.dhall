let commandWrapper = ../library.dhall

let home = "${env:HOME as Text}"

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
    → λ(language : < Bash | Dhall | Haskell >)
    → merge
      { Haskell =
          { targetFile = haskellSubcommandFileName wrapper command
          , executable = False
          , template =
                ./haskell-skel.dhall
              ? commandWrapper.config.skel.default-haskell-skel
          }
      , Bash =
          { targetFile = bashSubcommandFileName wrapper command
          , executable = True
          , template =
                ./bash-skel.dhall
              ? commandWrapper.config.skel.default-bash-skel
          }
      , Dhall =
          { targetFile = dhallConfigFileName wrapper command
          , executable = False
          , template =
              ( ./dhall-skel.dhall
              ? commandWrapper.config.skel.default-dhall-skel
              )
              wrapper
              subcommand
          }
      }
      language
