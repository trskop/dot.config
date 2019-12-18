let CommandWrapper = ./library.dhall

let Exec = ./exec/library.dhall

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

let fzf =
        λ(query : Optional Text)
      → CommandWrapper.CommandWithEnvironment::{
        , command = "fzf"
        , arguments =
            Exec.fzf.Options.toArguments
              Exec.fzf.Options::{
              , query = query
              , layout =
                  Some
                    < BottomOfTheScreen
                    | TopOfTheScreen
                    | TopOfTheScreenPromptAtTheBottom
                    >.TopOfTheScreen
              , height =
                  Some
                    (< Lines : Natural | Percentage : Natural >.Percentage 40)
              }
        }

in  CommandWrapper.CdConfig::{
    , directories = directories
    , menuTool = Some fzf
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
