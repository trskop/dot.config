let SystemInfo = ./SystemInfo.dhall

let osSpecificPackages =
      { DebianLinux =
            λ(_ : {})
          → [ "apt-listbugs"
            ]

      , BuntishLinux =
            λ(_ : {})
          → [] : List Text
      }

in
  λ(os : SystemInfo.Os)
  -- Store /etc in version control.  Git here is for 'etckeeper', but to also
  -- mark it as manually installed.
→ [ "etckeeper", "git"

  -- Essential for most of these tools to work reliably when used under
  -- non-root user.
  , "sudo"

  -- Most of this tooling is in Haskell.  Stack provides a way how to run
  -- Haskell scripts and istall rest of the tooling.
  , "haskell-stack"

  -- Access repositories via HTTPS.  Hopefully most Debian-like systems have
  -- this by default now.
  , "apt-transport-https"

  -- Be aware of possible issues on Debian/Buntish, especially useful when
  -- running on testing/unstable.
  --
  -- Package `apt-listbugs` is Debian specific, see `osSpecificPackages`.
  , "apt-listchanges"
  ]
  # merge osSpecificPackages os
