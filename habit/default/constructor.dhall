let CommandWrapper = ../../command-wrapper/library.dhall

in  CommandWrapper.ToolsetConfig::{
    , searchPath = [ "${env:HOME as Text}/.local/lib/habit" ]
    , manPath = [ "${env:HOME as Text}/.local/share/man" ]
    }
