let CommandWrapper = ./Types.dhall

let commandWrapper = ./library.dhall

let home = env:HOME as Text

let config = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let lib = "${home}/.local/lib"

let defaults = commandWrapper.config.skel.defaults

in    λ(toolset : Text)
    → λ(subcommand : Text)
    → λ(command : Text)
    →     defaults
          (   λ(language : CommandWrapper.SkelLanguage)
            → merge
              { Haskell =
                  { targetFile =
                      "${config}/${toolset}/toolset/app-${command}/Main.hs"
                  , executable =
                      False
                  , template =
                        ./haskell-skel.dhall
                      ? commandWrapper.config.skel.default-haskell-skel
                  }
              , Bash =
                  { targetFile =
                      "${lib}/${toolset}/${command}"
                  , executable =
                      True
                  , template =
                        ./bash-skel.dhall
                      ? commandWrapper.config.skel.default-bash-skel
                  }
              , Dhall =
                  { targetFile =
                      "${config}/${toolset}/${command}.dhall"
                  , executable =
                      False
                  , template =
                      (   ./dhall-skel.dhall
                        ? commandWrapper.config.skel.default-dhall-skel
                      )
                      toolset
                      subcommand
                  }
              }
              language
          )
        //  { editAfterwards =
                True
            , defaultLanguage =
                Some CommandWrapper.SkelLanguage.Bash
            }
      : CommandWrapper.SkelConfig
