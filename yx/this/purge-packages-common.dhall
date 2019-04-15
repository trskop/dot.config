let SystemInfo = ./SystemInfo.dhall

let osSpecificPackages =
      { DebianLinux =
            λ(_ : {})
          → [] : List Text

      , BuntishLinux =
            λ(_ : {})
          → [] : List Text
      }

in
  λ(os : SystemInfo.Os)
  -- Get rid of all the abominations:
→ [ "nano", "nano-tiny"

  -- These are now handled by Nix only:
  , "plantuml", "direnv", "shellcheck", "docker-compose"
  ] # merge osSpecificPackages os
