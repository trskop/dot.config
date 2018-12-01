let DockerExecOptions = ./DockerExecOptions.dhall

let DockerRunOptions = DockerExecOptions
      ⩓ { remove : Bool
        }

in  DockerRunOptions
