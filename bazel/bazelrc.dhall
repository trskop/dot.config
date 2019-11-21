-- vim: filetype=dhall
-- 
-- Bazel documentation related to `.bazelrc` files can be found at:
-- <https://docs.bazel.build/versions/master/guide.html#bazelrc>

let config = env:XDG_CONFIG_HOME as Text ? "${env:HOME as Text}/.config"

in  ''
    try-import ${config}/bazel/local.bazelrc
    ''
