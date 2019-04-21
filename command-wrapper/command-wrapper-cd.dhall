let CommandWrapper = ./lib/Types.dhall

let commandWrapper = ./lib/lib.dhall

let commonDirectories =
      ./cd/directories-common.dhall : List Text

let empty = [] : List Text

let directories =
      (./cd/directories-local.dhall ? empty) # (./cd/directories.dhall ? empty)

let customise =
        λ(defaults : CommandWrapper.CdConfig)
      → defaults
        //  { directories =
                defaults.directories # commonDirectories # directories

            , menuTool = (./cd/finder.dhall).fzf

            , terminalEmulator =
                let terminalEmulator =
                      { DebianLinux =
                            λ(_ : {})
                          → λ(directory : Text)
                          → commandWrapper.terminalEmulator.kitty (Some directory)

                      , BuntishLinux =
                            λ(_ : {})
                          → defaults.terminalEmulator
                      }

                in  merge terminalEmulator (../yx/this/system-info.dhall).os

            } : CommandWrapper.CdConfig

in commandWrapper.mkCdConfig customise
