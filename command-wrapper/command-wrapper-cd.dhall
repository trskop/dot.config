let CommandWrapper = ./Types.dhall

let commandWrapper = ./library.dhall

let emptyDirectories = commandWrapper.config.cd.emptyDirectories

let directories : List Text =
        ./cd/directories-common.dhall
      # (./cd/directories-local.dhall ? emptyDirectories)
      # (./cd/directories.dhall ? emptyDirectories)

let defaults = commandWrapper.config.cd.defaults

in  defaults
    //  { directories = defaults.directories # directories

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
