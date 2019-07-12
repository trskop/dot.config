let lib =
      ~/.local/src/github.com/trskop/command-wrapper/dhall/Exec/package.dhall
      sha256:5de94cb0d93f16618ca492456e8240cf154b8a7229c063e453407788e145f585

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
