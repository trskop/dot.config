User configuration files (dot–files)
====================================

* CommandWrapper configuration:
    * Default CommandWrapper configuration and configuration of stock
      subcommands: [command-wrapper/README.md](command-wrapper/README.md)
    * Toolset specialised for work environment:
      [habit/README.md](habit/README.md)
    * Toolset optimised for personal use: [yx/README.md](yx/README.md)

* GHCi configuration, see [ghc/README.md](ghc/README.md) for more information.

* Git configuration, see [git/README.md](git/README.md) for more information.

* Haskeline configuration, see [haskeline/README.md](haskeline/README.md) for
  more information.

* PostgreSQL interactive terminal (`psql`) configuration, see [psql/README.md
  ](psql/README.md) for more information.

* Readline configuration, see [readline/README.md](readline/README.md) for more
  information.

* Tmux configuration, see [tmux/README.md](tmux/README.md) for more
  information.

TODO:

* Merge <https://github.com/trskop/dot.config.nvim> into this repository.
* Merge <https://github.com/trskop/dot.xmonad> into this repository.


Installation
------------

```Bash
cd ~/.config/
git init
git remote add origin git@github.com:trskop/dot.config.git
git fetch --all
git checkout -b master origin/master
~/.config/dot.config.init.hs
```


Other Resources
---------------

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
