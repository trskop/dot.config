let CommandWrapper = ./library.dhall

let systemInfo = ../yx/this/system-info.dhall

let emptyDirectories = CommandWrapper.CdConfig.emptyDirectories

let directories
    : List Text
    =   ./cd/directories-common.dhall
      # (./cd/directories-local.dhall ? emptyDirectories)
      # (./cd/directories.dhall ? emptyDirectories)
      # (   ~/.local/src/localhost/dot.config/command-wrapper/cd/directories-local.dhall
          ? emptyDirectories
        )

in  CommandWrapper.CdConfig::{
    , directories = directories
    , menuTool =
        Some
          (   λ(query : Optional Text)
            → let fzf = CommandWrapper.CdConfig.menu-tool.fzf query

              in  fzf ⫽ { arguments = [ "--height=40%" ] # fzf.arguments }
          )
    , terminalEmulator =
        let terminalEmulator =
              { DebianLinux =
                    λ(directory : Text)
                  → CommandWrapper.TerminalEmulator.kitty (Some directory)
              , BuntishLinux =
                    λ(directory : Text)
                  → CommandWrapper.TerminalEmulator.urxvt (Some directory)
              }

        in  Some (merge terminalEmulator systemInfo.os)
    }
