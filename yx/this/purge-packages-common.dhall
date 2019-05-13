let SystemInfo = ./SystemInfo.dhall

let osSpecificPackages =
      { DebianLinux =
          [] : List Text

      , BuntishLinux =
          [] : List Text
      }

in
  λ(os : SystemInfo.Os)
  -- Get rid of all the abominations:
→ [ "nano", "nano-tiny"

  -- These are now handled by Nix only:
  , "plantuml", "direnv", "shellcheck", "docker-compose"
  ] # merge osSpecificPackages os
