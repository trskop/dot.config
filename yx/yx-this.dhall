let empty = [] : List Text

-- TODO: Implement this functionality.
let backup = ./this/backup-common.dhall # (./this/backup-local.dhall ? empty)

let UpdateAction =
      < UpdateSystem : {}
      | InstallPackages : {}
      | UpdateUserEnvironment : {}
      >

in  { defaults =
        -- Empty list is the same thing as specifying all values.
        { update = [UpdateAction.UpdateUserEnvironment {=}] : List UpdateAction
        }

    , system =
        { bootstrapPackages =
            ./this/bootstrap-packages-common.dhall # (./this/bootstrap-packages-local.dhall ? empty)

        , purgePackages =
            ./this/purge-packages-common.dhall # (./this/purge-packages-local.dhall ? empty)

        , packages =
            ./this/packages-common.dhall # (./this/packages-local.dhall ? empty)

        , timezone = "Europe/London"
        }

    , nix =
        { packages =
            ./this/nix-packages-common.dhall # (./this/nix-packages-local.dhall ? empty)
        }
    }
