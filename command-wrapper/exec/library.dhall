let lib =
      ~/.local/src/github.com/trskop/command-wrapper/dhall/Exec/package.dhall
      sha256:81cd59b81121a899dbb495ba855fb2fe8bf59d2ba13ea3d03c3ddfdb4eefc20a

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
