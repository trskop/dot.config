let CommandWrapper = ../library.dhall

let defaults = CommandWrapper.ToolsetConfig.default

in  CommandWrapper.ToolsetConfig::{
    , aliases = defaults.aliases # ./aliases-common.dhall # ./exec-aliases.dhall
    , searchPath =
        CommandWrapper.ToolsetConfig.defaultSearchPath
          env:HOME as Text
          "command-wrapper"
    }
