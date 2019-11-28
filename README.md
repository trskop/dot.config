User configuration files (dot–files)
====================================

*   [`bash/`](./bash/) – Configuration for Bash shell, most notablly
    `~/.bashrc`, i.e. [`bash/dot.bashrc`](./bash/dot.bashrc).

*   [`bat/`](./bat/) – Configuration for `bat` command, see
    [`bat/README.md`](./bat/README.md) for more information.

*   [`bazel/`](./bazel) – Bazel build system user-specific configuration.
    See [`baze/README.md`](./bazel/README.md)

*   [`command-wrapper/`](./command-wrapper/) – Global Command Wrapper
    configuration.  See [`command-wrapper/README.md`
    ](./command-wrapper/README.md) for more information.

*   [`ghc/`](./ghc/) – GHCi configuration, see
    [`ghc/README.md`](./ghc/README.md) for more information.

*   [`git/`](./git/) – Git configuration, see
    [`git/README.md`](./git/README.md) for more information.

*   [`habit/`](./habit/) – Configuration of Command Wrapper toolset optimised
    for work environment.  See [`habit/README.md`](./habit/README.md) for more
    details.

*   [`haskeline/`](./haskeline/) – Haskeline configuration, see
    [`haskeline/README.md`](./haskeline/README.md) for more information.

*   [`kitty/`](./kitty/) – Kitty terminal emulator configuration, see
    [`kitty/README.md`](./kitty/README.md).

*   [`nvim/`](./nvim/) – Neovim configuration, see
    [`nvim/README.md`](./nvim/README.md) for more information.

*   [`psql/`](./psql/) – PostgreSQL interactive terminal (`psql`) configuration,
    see [`psql/README.md`](./psql/README.md) for more information.

*   [`readline/`](./readline/) – Readline configuration, see
    [`readline/README.md`](./readline/README.md) for more information.

*   [`ripgrep/`](./ripgrep/) – Ripgrep configuration, see
    [`ripgrep/README.md`](./ripgrep/README.md).

*   [`sensible-editor/`](./sensible-editor/) – Configuration files for
    `sensible-editor`, a Debian-specific tool.  See `sensible-editor(1)` and
    [`sensible-editor/README.md`](./sensible-editor/README.md) for more
    information.

*   [`stack/`](./stack/) – Haskell Stack, see
    [`stack/README.md`](./stack/README.md).

*   [`tmux/`](./tmux/) – Tmux configuration, see
    [`tmux/README.md`](./tmux/README.md) for more information.

*   [`yx/`](./yx/) – Configuration of Command Wrapper toolset optimised for
    personal use.  See [`yx/README.md`](./yx/README.md) for more details.

*   [user-dirs.dirs](./user-dirs.dirs) and
    [user-dirs.locale](./user-dirs.locale) are XDG configuration files for
    special user directories.  See `xdg-user-dir(1)` and
    `xdg-user-dirs-update(1)` for more information.

*   [`Xresources`](./Xresources) – `~/.Xresources` configuration file.

TODO:

* Merge <https://github.com/trskop/dot.xmonad> into this repository.


Installation
------------

Some dependencies need to be installed.  On Debian it is:

```Bash
sudo apt install haskell-stack curl libtinfo-dev zlib1g-dev
```

We are expecting that there are some files already present in your `~/.config/`
directory.  This installation process will leave them untouched, but you may
need to resolve issues with files that exist both locally and in the
repository.

```Bash
cd ~/.config/
git init
git remote add origin git@github.com:trskop/dot.config.git
git fetch --all
git checkout -b master origin/master
```

If installing on a clean system then `~/.config` directory may not exist, in
which case it becomes a little bit easier.  We'll be using HTTPS protocol
instead of SSH, since SSH key for accessing GitHub may not be present on a
pristine system.  Just reconfigure remote URL afterwards.

```Bash
git clone https://github.com/trskop/dot.config.git ~/.config
```

Now we need to install everything:

```Bash
~/.config/dot.config.init.hs
~/.config/yx/toolset/install
yx this -Uusin
```


Version-controlled Host-specific Configuration
----------------------------------------------

Host-specific configuration is stored in `~/.local/src/localhost` repository
with following directory structure:

```
~/.local/src/localhost
├── ${host}/
│   └── dot.config
│       └── ...
│
├── dot.config -> ${host}/dot.config
└── this -> ${host}
```

Where `${host}` is the result of `hostname --fqdn`.  This allows us to
reuse/share the repository for multiple machines.

Script [`init-host-specific-config-repo.bash`
](./init-host-specific-config-repo.bash) is provided to create the repository
with the above structure.  It can be invoked as:

```Bash
bash ~/.config/init-host-specific-config-repo.bash
```


Other Resources
---------------

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [ArchWiki: XDG Base Directory
  ](https://wiki.archlinux.org/index.php/XDG_Base_Directory)
