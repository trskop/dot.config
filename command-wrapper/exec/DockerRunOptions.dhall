let
    DockerExecOptions =
      ./DockerExecOptions.dhall
      sha256:8de3098208653ed27e9890ccea2a5d70bad199c149eb5b93edcf951d6407ec8a

in let
    DockerRunOptions = DockerExecOptions
      ⩓ { remove : Bool
        }

in  DockerRunOptions
