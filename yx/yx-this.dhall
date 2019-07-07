let empty = [] : List Text

-- TODO: Implement this functionality.
let backup = ./this/backup-common.dhall # (./this/backup-local.dhall ? empty)

let system-info = ./this/system-info.dhall

let UpdateAction =
      < UpdateSystem
      | InstallPackages
      | UpdateUserEnvironment
      | UpdateNixEnvironment
      >

in  { defaults =
        -- Empty list is the same thing as specifying all values.
        { update = [UpdateAction.UpdateUserEnvironment] : List UpdateAction
        }

    , system =
        { bootstrapPackages =
              ./this/bootstrap-packages-common.dhall system-info.os
            # (./this/bootstrap-packages-local.dhall ? empty)

        , purgePackages =
              ./this/purge-packages-common.dhall system-info.os
            # (./this/purge-packages-local.dhall ? empty)

        , packages =
              ./this/packages-common.dhall system-info.os
            # (./this/packages-local.dhall ? empty)

        , timezone = "Europe/London"
        }

    , nix =
        { packages =
              ./this/nix-packages-common.dhall
            # (./this/nix-packages-local.dhall ? empty)
        }
    }
