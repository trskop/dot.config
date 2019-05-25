let CommandWrapper = ./Types.dhall

let commandWrapper = ./library.dhall

let commonDirectories =
      ./cd/directories-common.dhall : List Text

let emptyDirectories = commandWrapper.config.cd.emptyDirectories

let directories =
        (./cd/directories-local.dhall ? emptyDirectories)
      # (./cd/directories.dhall ? emptyDirectories)

let defaults = commandWrapper.config.cd.defaults

in  defaults
    //  { directories =
            defaults.directories # commonDirectories # directories

        , menuTool =
              λ(query : Optional Text)
            → let fzf = commandWrapper.config.cd.menuTools.fzf query
              in  fzf
                  //  { arguments = ["--height=40%"] # fzf.arguments
                      }

        , terminalEmulator =
            let terminalEmulator =
                  { DebianLinux =
                        λ(directory : Text)
                      → commandWrapper.terminalEmulator.kitty (Some directory)

                  , BuntishLinux =
                      defaults.terminalEmulator
                  }

            in  merge terminalEmulator (../yx/this/system-info.dhall).os

        } : CommandWrapper.CdConfig
