User configuration files (dot–files)
====================================

* CommandWrapper configuration:
    * Default CommandWrapper configuration and configuration of stock
      subcommands: [`command-wrapper/README.md`](command-wrapper/README.md)
    * Toolset specialised for work environment:
      [`habit/README.md`](habit/README.md)
    * Toolset optimised for personal use: [`yx/README.md`](yx/README.md)

* GHCi configuration, see [`ghc/README.md`](ghc/README.md) for more information.

* Git configuration, see [`git/README.md`](git/README.md) for more information.

* Haskeline configuration, see [`haskeline/README.md`](haskeline/README.md) for
  more information.

* PostgreSQL interactive terminal (`psql`) configuration, see [`psql/README.md`
  ](psql/README.md) for more information.

* Readline configuration, see [`readline/README.md`](readline/README.md) for more
  information.

* Tmux configuration, see [`tmux/README.md`](tmux/README.md) for more
  information.

* Neovim configuration, see [`nvim/README.md`](nvim/README.md) for more
  information.

* `~/.Xresources`, see [`Xresources`](Xresources) for details.

TODO:

* Merge <https://github.com/trskop/dot.xmonad> into this repository.


Installation
------------

Some dependencies need to be installed.  On Debian it is:

```Bash
sudo apt install haskell-stack curl libtinfo-dev zlib1g-dev
```

We are expecting that there are some files already present in your `~/.config/`
directory.  This installation process will leave them untached, but you may
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
git clone https://github.com/trskop/dot.config.git
```

Now we need to install everything:

```Bash
~/.config/dot.config.init.hs
~/.config/yx/toolset/install
yx this -Uusi
cat > .bashrc <<EOF
# shellcheck shell=bash

# ~/.bashrc: executed by bash(1) for non-login shells.
#
# See /usr/share/doc/bash/examples/startup-files (in the package bash-doc) for
# examples.

# If not running interactively, don't do anything.
case $- in
    *i*) ;;
      *) return;;
esac

# shellcheck source=/dev/null
[[ -x "${HOME}/.local/bin/genbashrc" ]] && source <("${HOME}/.local/bin/genbashrc")
EOF
```


Other Resources
---------------

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [ArchWiki: XDG Base Directory
  ](https://wiki.archlinux.org/index.php/XDG_Base_Directory)
