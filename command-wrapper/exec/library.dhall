let lib =
      ~/.local/src/github.com/trskop/command-wrapper/dhall/Exec/package.dhall
      sha256:6acc7297e36111a4b70eac52d9a357c0d1934e8c3086758dcf2d2837292e5fd6

in    lib
    //  { completion =
              lib.completion
            //  { bazel =
                    ./completion/bazel
                , docker-compose =
                    ./completion/docker-compose
                , yarn =
                    ./completion/yarn
                }
        }
