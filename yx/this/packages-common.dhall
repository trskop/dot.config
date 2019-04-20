let SystemInfo = ./SystemInfo.dhall

let osSpecificPackages =
      { DebianLinux =
            λ(_ : {})
          → [ "neovim/unstable", "lua-nvim"
            -- Neovim Python provider is required by some plugins that I'm using:
            , "python-neovim", "python3-neovim"

            -- Neovim Python provider dependencies for the case when they
            -- needed to be installed via Pip:
            --, "python-dev", "python-pip"
            --, "python3-dev", "python3-pip"
            ]

      , BuntishLinux =
            λ(_ : {})
            -- https://github.com/neovim/neovim/wiki/Installing-Neovim#ubuntu
          → [ "neovim"
            , "python-dev", "python-pip"
            , "python3-dev", "python3-pip"

            -- Currently doesn't work on Debian:
            , "gcc-doc"
            ] : List Text
      }

in
  λ(os : SystemInfo.Os)

  -- {{{ System ---------------------------------------------------------------

→ [ "psmisc"  -- Contains pstree.
  , "cifs-utils"  -- Utilities for managing mounts of SMB/CIFS (Common
                  -- Internet File System) filesystems.

  -- }}} System ---------------------------------------------------------------
  -- {{{ Editors --------------------------------------------------------------

  -- Use more advanced fallback editor.
  , "vim-nox", "vim-doc"

  -- Neovim installed as a default editor, see `osSpecificPackages` section.

  -- }}} Editors --------------------------------------------------------------
  -- {{{ Terminal emulators and multiplexers ----------------------------------

  , "rxvt-unicode-256color"
  , "screen", "tmux"

  -- }}} Terminal emulators and multiplexers ----------------------------------
  -- {{{ Command line utilities -----------------------------------------------

  , "curl", "wget"

  , "tree"

  , "zip"
  , "p7zip-full", "p7zip-rar" -- Command line version of 7-Zip file archiver.

  , "xinput"  -- Utility to configure and test X input devices.

  , "xclip", "xsel" -- Command line utilities for clipboard manipulation.

  , "renameutils" -- Renaming utilities designed to make renaming of files
                  --   faster and less cumbersome. Includes "qmv" command.

  , "librsvg2-bin"  -- Command-line utility to convert the SVG files to the
                    --   PNG format and a graphical SVG viewer.

  , "jq"  -- JSON processor; like sed for JSON data.

  , "inotify-tools" -- Command-line programs providing a simple interface
                    --   to inotify.

  , "ftp"  -- Simple command-line FTP client.

  , "colordiff" -- Wrapper for diff command which adds clours.

  , "apache2-utils"  -- Contains 'host' DNS lookup utility.

  , "graphviz"

  -- Using Nix fore these:
  --, "pandoc"
  --, "plantuml"  -- Compile text-based UML diagrams into other formats

  -- }}} Command line utilities -----------------------------------------------
  -- {{{ Networking -----------------------------------------------------------

  , "net-tools"  -- Contains 'netstat'.

  , "netcat-traditional"  -- A simple Unix utility which reads and writes data
                          --   across network connections using TCP o UDP
                          --   protocol.

  , "netcat-openbsd"  -- OpenBSD rewrite of netcat, it includes support for
                      --   IPv6, proxies, and Unix sockets

  , "socat"  -- Socat (for SOcket CAT) establishes two bidirectional byte
             --   streams and transfers data between them. Data channels may
             --   be files, pipes, devices (terminal or modem, etc.), or
             --   sockets (Unix, IPv4, IPv6, raw, UDP, TCP, SSL). It provides
             --   forking, logging and tracing, different modes for
             --   interprocess communication and many more options.

  , "corkscrew"  -- A simple tool to tunnel TCP connections through an HTTP
                 --   proxy supporting the CONNECT method. It reads stdin and
                 --   writes to stdout during the connection, just like netcat.

  -- }}} Networking -----------------------------------------------------------
  -- {{{ Development ----------------------------------------------------------

  , "build-essential"
  -- , "gcc-doc"  -- Includes manual pages for gcc.
  , "git", "git-doc", "git-gui", "gitk"
  , "sqlite3"
  , "strace"
  , "libssl-dev"

  -- {{{ Development -- Haskell -----------------------------------------------

  , "haskell-stack"
  , "threadscope"
  , "zlib1g-dev"    -- Required by e.g. "stack upgrade".
  , "libtinfo-dev"  -- Required by e.g. Haskell IDE Engine compilation.

  -- }}} Development -- Haskell -----------------------------------------------
  -- }}} Development ----------------------------------------------------------
  -- {{{ Desktop --------------------------------------------------------------

  , "dia" -- An editor for diagrams, graphs, charts etc.
  ]
  -- }}} Desktop --------------------------------------------------------------