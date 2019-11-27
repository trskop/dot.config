-- vim: filetype=dhall
--
-- Bazel documentation related to `.bazelrc` files can be found at:
-- <https://docs.bazel.build/versions/master/guide.html#bazelrc>

let home = env:HOME as Text

let config = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let hostSpecificConfig = "${home}/.local/src/localhost/dot.config"

in  ''
    # Machine-specific, i.e. local, configuration.  It contains options that
    # are not transferable between hosts.
    try-import ${hostSpecificConfig}/bazel/local.bazelrc
    try-import ${config}/bazel/local.bazelrc
    ''
