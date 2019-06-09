let CommandWrapper = ../Types.dhall

let commandWrapper = ../library.dhall

let defaults = commandWrapper.config.toolset.defaults

let helpMessage = ./help-common.txt as Text

in        defaults
      //  { aliases =
              defaults.aliases # ./aliases-common.dhall # ./exec-aliases.dhall

          , searchPath =
              commandWrapper.config.toolset.defaultSearchPath
              env:HOME as Text
              "command-wrapper"

          , extraHelpMessage = Some
              (   Optional/fold Text defaults.extraHelpMessage Text
                    (λ(t : Text) → "${t}\n")
                    ""
              ++  helpMessage
              )
          }
    : CommandWrapper.ToolsetConfig
