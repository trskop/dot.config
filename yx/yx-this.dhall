let purge = ./this/purge-packages.dhall ? ([] : List Text)

let packages = ./this/packages.dhall ? ([] : List Text)

let UpdateAction =
      < UpdateSystem : {}
      | InstallPackages : {}
      | UpdateUserEnvironment : {}
      >

in  { bootstrapPackages =
        -- Store /etc in version control.  Git here is for 'etckeeper', but to
        -- also mark it as manually installed.
        [ "etckeeper", "git"

        -- Essential for most of these tools to work reliably when used under
        -- non-root user.
        , "sudo"

        -- Most of this tooling is in Haskell.  Stack provides a way how to run
        -- Haskell scripts and istall rest of the tooling.
        , "haskell-stack"

        -- Access repositories via HTTPS.  Hopefully most Debian-like systems
        -- have this by default now.
        , "apt-transport-https"
        ] : List Text

    -- Get rid of all the abominations.
    , purgePackages = ([ "nano" ] : List Text) # purge

    , packages =
        -- Be aware of possible issues on Debian, especially useful when
        -- running on testing/unstable.
        [ "apt-listbugs", "apt-listchanges"

        -- Use more advanced fallback editor.
        , "vim-nox", "vim-doc"
        ] # packages

    , timezone = "Europe/London"

    , defaults =
        -- Empty list is the same thing as specifying all values.
        { update = [UpdateAction.UpdateUserEnvironment {=}] : List UpdateAction
        }
    }
