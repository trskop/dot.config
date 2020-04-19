let CommandWrapper = ../../command-wrapper/library.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let home = env:HOME as Text

let xdgConfigDir = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let script =
      ''
      ${xdgConfigDir}/scripts/dhall-lsp-server-wrapper
      sha256:382fbb508caa1421e14c46e7a74d9f535ca09fd9875921f61e7484e8b3e05c26
      as Text
      ''

in  CommandWrapper.ExecNamedCommand::{
    , name = "dhall-lsp-server"
    , description =
        Some "Call 'dhall-lsp-server', but download it first if not already."
    , command =
          λ(_ : CommandWrapper.Verbosity.Type)
        → λ(_ : CommandWrapper.ColourOutput.Type)
        → λ(arguments : List Text)
        → CommandWrapper.ExecCommand::{
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
