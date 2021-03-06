let CommandWrapper = ./library.dhall

let home = env:HOME as Text

let config = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let lib = "${home}/.local/lib"

in    λ(toolset : Text)
    → λ(subcommand : Text)
    → λ(command : Text)
    → CommandWrapper.SkelConfig::{
      , template =
            λ(language : CommandWrapper.SkelConfig.SkelLanguage)
          → merge
              { Haskell =
                  { targetFile =
                      "${config}/${toolset}/toolset/app-${command}/Main.hs"
                  , executable = False
                  , template =
                        ./skel/haskell-template.dhall
                      ? CommandWrapper.SkelConfig.template.haskell
                  }
              , Bash =
                  { targetFile = "${lib}/${toolset}/${command}"
                  , executable = True
                  , template =
                        ./skel/bash-template.dhall
                      ? CommandWrapper.SkelConfig.template.bash
                  }
              , Dhall =
                  { targetFile = "${config}/${toolset}/${command}.dhall"
                  , executable = False
                  , template =
                        ./skel/dhall-template.dhall
                      ? CommandWrapper.SkelConfig.template.dhall
                  }
              }
              language
      , editAfterwards = True
      , defaultLanguage = Some CommandWrapper.SkelConfig.SkelLanguage.Bash
      }
