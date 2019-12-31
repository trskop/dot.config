let CommandWrapper = ../../command-wrapper/library.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let script =
      ''
      https://raw.githubusercontent.com/trskop/dot.config/e89a4a625fca95220447956d3bdee0afeb4aa4d2/scripts/dhall-lsp-server-wrapper
      sha256:c12bd9e3b5423c8c5b992b0da928cdfc856fd911d5c4afdd0fc42f2a9f239829
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
