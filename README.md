# ~/.config/nvim

Personalised Neovim configuration.


## Installation


### Linux

Install *Neovim* and *Python 2 & 3* language servers using your package
manager. If Python language servers aren't packaged then make sure to install
`pip` for both Python versions, and do following:

```Bash
pip2 install neovim
pip3 install neovim
```

Install Powerline fonts:

```Bash
git clone https://github.com/powerline/fonts.git ~/.local/src/powerline/fonts
cd ~/.local/src/powerline/fonts
# Review install.sh before executing it.
./install.sh
```

Configure terminal emulator to use e.g. *DejaVu Sans Mono for Powerline*. For
Urxvt it can be by done in `.Xresources`/`.Xdefaults`, e.g.:

```
URxvt.font: xft:DejaVu Sans Mono for Powerline:size=14:style=Book
```

Install *Haskell IDE Engine* using [official installation instructions
](https://github.com/haskell/haskell-ide-engine#installation).


### Mac OS X

Install Python and Neovim hosts for it:

```Bash
brew install python python3
pip2 install neovim
pip3 install neovim
```

Install Neovim:

```Bash
brew install neovim
```

Install powerline fonts:

```Bash
git clone https://github.com/powerline/fonts.git ~/.local/src/powerline/fonts
```

Open "Font Book" (press `command-space` and start searching for it), then press
"+" button to add fonts. In the finder window press `command-shift-.` to see
hidden files and search for `.local/src/powerline/fonts` in your home
directory. Press "Open".

*Haskell IDE Engine* dependencies:

```Bash
brew install icu4c
```

Install *Haskell IDE Engine*:

* [Official instructions for installing *Haskell IDE Engine*
  ](https://github.com/haskell/haskell-ide-engine#installation)

* When using Brew the build command had to be modified. Installation example:

    ```Bash
    git clone --recursive https://github.com/haskell/haskell-ide-engine ~/.local/src/haskell/haskell-ide-engine
    cd ~/.local/src/haskell/haskell-ide-engine
    stack [--stack-yaml=FILE] --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include install
    ```


### All Platforms

Don't forget that `~/.local/bin` has to be in your `PATH`.

Configure Neovim:

```Bash
git clone git@github.com:trskop/dot.config.nvim.git ~/.config/nvim
cd ~/.config/nvim
wget https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh
# Review installer.sh before executing it.
sh installer.sh dein.vim
nvim -c ':helptags ~/.config/nvim/doc/ | :UpdateRemotePlugins'
```


## Notes

Somewhat annotated and incomplete list of used Neovim plugins and integrated
command line tools. See [`init.vim`](init.vim) for more.


### Git Integration (plugins)

[tpope/vim-fugitive](https://github.com/tpope/vim-fugitive)

[int3/vim-extradite](https://github.com/int3/vim-extradite) which extends
`vim-fugitive`

Ubuntu PPA with latest Git packages:
<https://launchpad.net/~git-core/+archive/ubuntu/ppa>

Personalised Git configuration:
[github.com/trskop/dot.config-git](https://github.com/trskop/dot.config-git)


### FZF (Fuzzy Finder) Integration (command line tool, plugins)

[junegunn/fzf](https://github.com/junegunn/fzf) (command line tool, plugin)

[junegunn/fzf.vim](https://github.com/junegunn/fzf.vim) (plugin)

See also [`init.vim`](init.vim) for more information. Fzf can be installed and
used outside of Neovim, e.g. for use in bash scripts.


### NERDTree (Hierarchical File System Browser) plugin

[github.com/scrooloose/nerdtree](https://github.com/scrooloose/nerdtree)
(plugin)

[github.com/Xuyuanp/nerdtree-git-plugin](https://github.com/Xuyuanp/nerdtree-git-plugin)
(plugin) which extends `scrooloose/nerdtree`.

See also [`init.vim`](init.vim) for more information.


### Syntax Highlighting for PureScript (plugin)

[github.com/purescript-contrib/purescript-vim](https://github.com/purescript-contrib/purescript-vim)

Installation and configuration: [README.md#installation](https://github.com/purescript-contrib/purescript-vim#installation).


### Better Syntax Highlighting for Haskell (plugin)

[github.com/neovimhaskell/haskell-vim](https://github.com/neovimhaskell/haskell-vim)

Installation and configuration: [README.md#installation](https://github.com/neovimhaskell/haskell-vim#installation).


### LSP (Language Server Protocol) Client for Neovim (plugin)

Pligin: [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)

Installation and configuration: [INSTALL.md](https://github.com/autozimu/LanguageClient-neovim/blob/next/INSTALL.md)


### Haskell IDE Engine (command line tool)

[github.com/haskell/haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)

Requires: *LanguageClient-neovim* plugin (see above)

Installation instructions:
[README.md#installation](https://github.com/haskell/haskell-ide-engine#installation)

Configuration instructions:

* *LanguageClient-neovim* configuration:
  [README.md#using-hie-with-neovim](https://github.com/haskell/haskell-ide-engine#using-hie-with-neovim)

* Docs on hover/completion:
  [README.md#docs-on-hovercompletion](https://github.com/haskell/haskell-ide-engine#docs-on-hovercompletion)


### PlantUML (command line tool, plugin)

[plantuml.com](http://plantuml.com) (command line tool)

[github.com/aklt/plantuml-syntax](https://github.com/aklt/plantuml-syntax)
(plugin)

Installation *PlantUML* command line tool on Debian-compatible systems:

```bash
apt install plantuml graphviz
```
