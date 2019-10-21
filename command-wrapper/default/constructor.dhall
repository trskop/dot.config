let CommandWrapper = ../library.dhall

let helpMessage = ./help-common.txt as Text

let defaults = CommandWrapper.ToolsetConfig.default

in    CommandWrapper.ToolsetConfig::{
      , aliases =
          defaults.aliases # ./aliases-common.dhall # ./exec-aliases.dhall

      , searchPath =
          CommandWrapper.ToolsetConfig.defaultSearchPath
          env:HOME as Text
          "command-wrapper"

      , extraHelpMessage = Some
          (   Optional/fold Text defaults.extraHelpMessage Text
                (λ(t : Text) → "${t}\n")
                ""
          ++  helpMessage
          )
      }
    : CommandWrapper.ToolsetConfig.Type
