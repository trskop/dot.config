let CommandWrapper = ../../command-wrapper/library.dhall

let home = env:HOME as Text

let xdgConfigDir = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

in  CommandWrapper.ExecNamedCommand::{
    , name = "rmapi"
    , description = Some "Call 'rmapi', but download it first if not already."
    , command =
        λ(_ : CommandWrapper.Verbosity.Type) →
        λ(_ : CommandWrapper.ColourOutput.Type) →
        λ(arguments : List Text) →
          CommandWrapper.ExecCommand::{
          , command = "/usr/bin/env"
          , arguments =
                [ "bash", "${xdgConfigDir}/scripts/rmapi-wrapper.bash" ]
              # arguments
          , searchPath = False
          }
    }
