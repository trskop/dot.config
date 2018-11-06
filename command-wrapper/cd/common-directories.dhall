let
    home = "${env:HOME as Text}"

in let
    config = "${home}/.config"

in let
    local = "${home}/.local"

in
    [ "${config}"
    , "${config}/command-wrapper"
    , "${config}/git"
    , "${config}/nvim"
    , "${config}/this"
    , "${config}/yx"
    , "${config}/yx/toolset"
    , "${local}/lib/command-wrapper"
    , "${local}/lib/yx"
    , "${local}/src/trskop/command-wrapper"
    , "${local}/src/trskop/genbashrc"
    , "${home}/Devel"
    , "${home}/Downloads"
    , "${home}/.ssh"
    ] : List Text
