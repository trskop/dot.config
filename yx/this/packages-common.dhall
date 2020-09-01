let SystemInfo = ./SystemInfo.dhall

let osSpecificPackages =
      { DebianLinux =
          -- Neovim is the editor of choice.
          [ "neovim/unstable", "lua-nvim"
          -- Neovim Python provider is required by some plugins that I'm using:
          , "python-neovim", "python3-neovim"

          -- Neovim Python provider dependencies for the case when they
          -- needed to be installed via Pip:
          --, "python-dev", "python-pip"
          --, "python3-dev", "python3-pip"

          -- Terminal emulator of choice.
          , "kitty", "kitty-doc"

          -- TODO: Test that these packages are available on Buntish distros:

          -- Fuzzy text selector like `fzf`, but simpler.  The algorithm, it
          -- uses, is slightly different, which may be better in some cases.
          , "fzy"

          -- Grep alternative that is faster and uses recursive search by
          -- default. <https://github.com/BurntSushi/ripgrep>
          -- There's also Vim/Neovim plugin for it.
          , "ripgrep"

          -- a variant of the UNIX `find` command that operates breadth-first
          -- rather than depth-first.
          , "bfs"

          -- Stream buffering tool that also shows the I/O rate and summary to
          -- the user.
          , "mbuffer"

          -- Simple, fast and user-friendly alternative to find.
          , "fd-find"

          -- Tools to record TTY session and convert it into a GIF image.
          , "ttyrec", "ttygif", "gifsicle"

          -- Mercurial is a SCM (Source Control Management) system. There are
          -- few old mercurial repos that I need to interact with.
          , "mercurial"
          ]

      , BuntishLinux =
          -- https://github.com/neovim/neovim/wiki/Installing-Neovim#ubuntu
          [ "neovim"
          , "python-dev", "python-pip"
          , "python3-dev", "python3-pip"
          ]
      }

in
  λ(os : SystemInfo.Os)

  -- {{{ System ---------------------------------------------------------------

→ -- Contains pstree.
  [ "psmisc"

  -- Utilities for managing mounts of SMB/CIFS (Common
  -- Internet File System) filesystems.
  , "cifs-utils"

  -- Tools to control and monitor control groups.
  , "cgroup-tools"

  -- }}} System ---------------------------------------------------------------
  -- {{{ Editors --------------------------------------------------------------

  -- Use more advanced fallback editor.
  , "vim-nox", "vim-doc"

  -- Neovim installed as a default editor, see `osSpecificPackages` section.

  -- }}} Editors --------------------------------------------------------------
  -- {{{ Terminal emulators and multiplexers ----------------------------------

  -- RXVT unicode is used as a fallback terminal emulator.
  , "rxvt-unicode-256color"
  , "screen", "tmux"

  -- }}} Terminal emulators and multiplexers ----------------------------------
  -- {{{ Command line utilities -----------------------------------------------

  , "curl", "wget"

  , "tree"

  , "zip"
  , "p7zip-full", "p7zip-rar" -- Command line version of 7-Zip file archiver.

  -- Utility to configure and test X input devices.
  , "xinput"

  -- Command line utilities for clipboard manipulation.
  , "xclip", "xsel"

  -- Renaming utilities designed to make renaming of files faster and less
  -- cumbersome. Includes "qmv" command.
  , "renameutils" 

  -- Command-line utility to convert the SVG files to the PNG format and a
  -- graphical SVG viewer.
  , "librsvg2-bin"

  -- JSON processor; like sed for JSON data.
  , "jq"

  -- Command-line programs providing a simple interface to inotify.
  , "inotify-tools"

  -- Simple command-line FTP client.
  , "ftp"

  -- Wrapper for diff command which adds clours.
  , "colordiff"

  -- Contains 'host' DNS lookup utility.
  , "apache2-utils"

  , "graphviz"

  -- Useful UUID commands built using libuuid. Contains `uuidgen`.
  , "uuid-runtime"

  -- Random password generator.
  , "pwgen"

  -- Reads input from stdin and allows data to be selected and copied to the
  -- clipboard.
  , "yank"

  -- Backup tool which stores the backups in a system based around the packfile
  -- format from Git. <https://bup.github.io/>
  , "bup"

  -- Using Nix fore these:
  --, "pandoc"
  --, "plantuml"  -- Compile text-based UML diagrams into other formats

  -- }}} Command line utilities -----------------------------------------------
  -- {{{ Networking -----------------------------------------------------------

  -- Contains 'netstat'.
  , "net-tools"

  -- A simple Unix utility which reads and writes data across network
  -- connections using TCP o UDP protocol.
  , "netcat-traditional"

  -- OpenBSD rewrite of netcat, it includes support for IPv6, proxies, and Unix
  -- sockets
  , "netcat-openbsd"

  -- Socat (for SOcket CAT) establishes two bidirectional byte streams and 
  -- transfers data between them. Data channels may be files, pipes, devices
  -- (terminal or modem, etc.), or sockets (Unix, IPv4, IPv6, raw, UDP, TCP,
  -- SSL). It provides forking, logging and tracing, different modes for
  -- interprocess communication and many more options.
  , "socat"

  -- A simple tool to tunnel TCP connections through an HTTP proxy supporting
  -- the CONNECT method. It reads stdin and writes to stdout during the
  -- connection, just like netcat.
  , "corkscrew"

  -- }}} Networking -----------------------------------------------------------
  -- {{{ Development ----------------------------------------------------------

  , "build-essential"
  , "gcc-doc"  -- Includes manual pages for gcc.
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
  , "mplayer"
  ]
  -- }}} Desktop --------------------------------------------------------------
  # merge osSpecificPackages os
