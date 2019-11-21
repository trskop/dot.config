-- vim: filetype=dhall
--
-- Bazel documentation related to `.bazelrc` files can be found at:
-- <https://docs.bazel.build/versions/master/guide.html#bazelrc>

let config = env:XDG_CONFIG_HOME as Text ? "${env:HOME as Text}/.config"

in  ''
    # Machine-specific, i.e. local, configuration.  It contains options that
    # are not transferable between hosts.
    try-import ${config}/bazel/local.bazelrc
    ''
