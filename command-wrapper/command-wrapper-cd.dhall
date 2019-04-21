let CommandWrapper = ./lib/Types.dhall

let commandWrapper = ./lib/lib.dhall

let context =
      { home = "${env:HOME as Text}"
      }

let commonDirectories =
      ./cd/directories-common.dhall : List Text

let empty = [] : List Text

let directories =
      (./cd/directories-local.dhall ? empty) # (./cd/directories.dhall ? empty)

let customise =
        λ(defaults : CommandWrapper.CdConfig)
      → { directories = defaults.directories # commonDirectories # directories
        , menuTool = ./cd/fzf.dhall (None Text)
        , shell = "${env:SHELL as Text ? "/bin/bash"}"
        , terminalEmulator =
            let terminalEmulator =
                  { DebianLinux =
                      -- On Debian we have Kitty available.  See
                      -- ../yx/this/packages-common.dhall for details.
                        λ(_ : {})
                      → λ(directory : Text)
                      →   commandWrapper.command.withExtraArguments
                            (commandWrapper.command.simple "kitty")
                            ["--directory", directory]
                        ∧ { environment =
                              [] : List CommandWrapper.EnvironmentVariable
                          }

                  , BuntishLinux = λ(_ : {}) → defaults.terminalEmulator
                  }

            in  merge terminalEmulator (../yx/this/system-info.dhall).os

        } : CommandWrapper.CdConfig

in commandWrapper.mkCdConfig context customise
