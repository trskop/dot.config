let
    DockerExecOptions =
      ./DockerExecOptions.dhall
      sha256:4c5fcb96707e8d3ce1395151e60ec7191310c733578578211ec73a4da1d9a98b

in let
    DockerRunOptions = DockerExecOptions
      //\\  { remove : Bool
            }

in  DockerRunOptions
