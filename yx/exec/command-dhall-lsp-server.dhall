let CommandWrapper = ../../command-wrapper/library.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let script =
      ''
      https://raw.githubusercontent.com/trskop/dot.config/2b589e34c18df01478b3997792229fd7e4fb9c98/scripts/dhall-lsp-server-wrapper
      sha256:f82defc79d7e9ca5058c34e0dec63e23343f8f8a02aa035da0b59288d818aa6a
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
