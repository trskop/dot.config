let CommandWrapper = ../../command-wrapper/library.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let home = env:HOME as Text

let xdgConfigDir = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let script =
      ''
      ${xdgConfigDir}/scripts/dhall-lsp-server-wrapper
      sha256:0887822387cc03bc6c8d1eae872b80371820167ae24ee1d3a8a773c2be5fafed
      as Text
      ''

in  CommandWrapper.ExecNamedCommand::{
    , name = "dhall-lsp-server"
    , description = Some
        "Call 'dhall-lsp-server', but download it first if not already."
    , command =
        λ(_ : CommandWrapper.Verbosity.Type) →
        λ(_ : CommandWrapper.ColourOutput.Type) →
        λ(arguments : List Text) →
          CommandWrapper.ExecCommand::{
          , command = toolset
          , arguments =
                [ "--no-aliases"
                , "config"
                , "--dhall-exec"
                , "--expression=${script}"
                , "--"
                ]
              # arguments
          }
    }
