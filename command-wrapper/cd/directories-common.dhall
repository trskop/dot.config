let home = "${env:HOME as Text}"

let config = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let local = "${home}/.local"

in  [ "${config}"
    , "${config}/command-wrapper"
    , "${config}/git"
    , "${config}/nvim"
    , "${config}/yx"
    , "${config}/yx/this"
    , "${config}/yx/toolset"
    , "${local}/lib/command-wrapper"
    , "${local}/lib/yx"
    , "${home}/Devel"
    , "${home}/Downloads"
    , "${home}/.ssh"
    ] : List Text
