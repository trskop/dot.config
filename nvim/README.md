# ~/.config/nvim

Personalised Neovim configuration.  Not everything is documented here, see also
[`init.vim`](./init.vim) and files in [`ftplugin/`](./ftplugin/).


## Installation


### Linux

Install *Neovim* and *Python 2 & 3* language servers using your package
manager. If Python language servers aren't packaged then make sure to install
`pip` for both Python versions, and do following:

```Bash
pip2 install neovim
pip3 install neovim
```

Install Powerline fonts, this is now done by
[`dot.config.init.hs`](../dot.config.init.hs):

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

Install *Haskell IDE Engine* or *ghcide*:

* Follow instructions in section [Haskell IDE Engine (command line tool)
  ](#haskell-ide-engine-command-line-tool) or in [ghcide (Haskell IDE tool)
  ](#ghcide-haskell-ide-tool).


#### Debian

Install *Neovim* and *Python 2 & 3* language servers:

```Bash
apt install python-neovim python3-neovim
```

Dependencies required by installation of Haskell IDE Engine:

```Bash
apt install libtinfo-dev
```


### Mac OS X

**WARNING: This hadn't been tested in a while and it may be out of date.**

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

Install *Haskell IDE Engine* or *ghcide*:

* Follow instructions in section [Haskell IDE Engine (command line tool)
  ](#haskell-ide-engine-command-line-tool).

* When using Brew, the build command had to be modified:

    ```Bash
    git clone --recursive https://github.com/haskell/haskell-ide-engine ~/.local/src/haskell/haskell-ide-engine
    cd ~/.local/src/haskell/haskell-ide-engine
    stack [--stack-yaml=FILE] --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include install
    ```


### All Platforms

Don't forget that `~/.local/bin` has to be in your `PATH`.

```Bash
nvim -c ':helptags ~/.config/nvim/doc/ | :UpdateRemotePlugins'
```


## Notes

Somewhat annotated and incomplete list of used Neovim plugins and integrated
command line tools. See [`init.vim`](init.vim) for more.

Custom commands and shortcuts are documented in `:help rituals`, i.e.
[`doc/rituals.txt`](rituals.txt).

Very useful tool for interacting with existing Neovim instance, especially from
`:terminal`, is [neovim-remote](https://github.com/mhinz/neovim-remote).


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

[github.com/Xuyuanp/nerdtree-git-plugin
](https://github.com/Xuyuanp/nerdtree-git-plugin) (plugin) which extends
`scrooloose/nerdtree`.

See also [`init.vim`](init.vim) for more information.


### Syntax Highlighting for PureScript (plugin)

[github.com/purescript-contrib/purescript-vim
](https://github.com/purescript-contrib/purescript-vim)

Installation and configuration: [purescript-vim/README.md#installation
](https://github.com/purescript-contrib/purescript-vim#installation).


### Better Syntax Highlighting for Haskell (plugin)

[github.com/neovimhaskell/haskell-vim
](https://github.com/neovimhaskell/haskell-vim)

Installation and configuration: [haskell-vim/README.md#installation
](https://github.com/neovimhaskell/haskell-vim#installation).


### LSP (Language Server Protocol) Client for Neovim (plugin)

Pligin: [github.com/autozimu/LanguageClient-neovim
](https://github.com/autozimu/LanguageClient-neovim)

Installation and configuration: [LanguageClient-neovim/INSTALL.md
](https://github.com/autozimu/LanguageClient-neovim/blob/next/INSTALL.md)

Alternatives to *LanguageClient-neovim*:

* [github.com/neoclide/coc.nvim](https://github.com/neoclide/coc.nvim)
* [github.com/prabirshrestha/vim-lsp](https://github.com/prabirshrestha/vim-lsp)


### Haskell IDE Engine (command line tool)

[github.com/haskell/haskell-ide-engine
](https://github.com/haskell/haskell-ide-engine)

Requires: [*LanguageClient-neovim*
](#lsp-language-server-protocol-client-for-neovim-plugin) plugin

Installation instructions: [haskell-ide-engine/README.md#installation
](https://github.com/haskell/haskell-ide-engine#installation)

Configuration instructions:

* *LanguageClient-neovim* configuration:
  [haskell-ide-engine/README.md#using-hie-with-neovim
  ](https://github.com/haskell/haskell-ide-engine#using-hie-with-neovim)

* Docs on hover/completion:
  [haskell-ide-engine/README.md#docs-on-hovercompletion
  ](https://github.com/haskell/haskell-ide-engine#docs-on-hovercompletion)

* Configuration of `hie-bios` used by Haskell IDE Engine:
  [hie-bios/README.md#explicit-configuration
  ](https://github.com/mpickering/hie-bios#explicit-configuration)

After installation users don't interact with Haskell IDE Engine directly, but
through [*LanguageClient-neovim*
](#lsp-language-server-protocol-client-for-neovim-plugin) plugin. See its
documentation (`:help LanguageClient`) and [`init.vim`](init.vim) for more
information.


### ghcide (Haskell IDE tool)

[github.com/digital-asset/ghcide](https://github.com/digital-asset/ghcide)

Requires: [*LanguageClient-neovim*
](#lsp-language-server-protocol-client-for-neovim-plugin) plugin

Configuration instructions:

* [ghcide/README.md#using-with-vimneovim
  ](https://github.com/digital-asset/ghcide#using-with-vimneovim)

* Configuration of `hie-bios` used by Haskell IDE Engine:
  [hie-bios/README.md#explicit-configuration
  ](https://github.com/mpickering/hie-bios#explicit-configuration)

Same caveats apply as for [Haskell IDE Engine (command line tool)
](#haskell-ide-engine-command-line-tool).


### PlantUML (command line tool, plugin)

[plantuml.com](http://plantuml.com) (command line tool)

[github.com/aklt/plantuml-syntax](https://github.com/aklt/plantuml-syntax)
(plugin)

Installation *PlantUML* command line tool on Debian-compatible systems:

```bash
apt install plantuml graphviz
```


## Interesting Articles

* [blog.jez.io/haskell-development-with-neovim
  ](https://blog.jez.io/haskell-development-with-neovim/) from 16th July, 2017,
  some of the stuff there is still relevant.

* [mendo.zone/fun/neovim-setup-haskell
  ](https://mendo.zone/fun/neovim-setup-haskell/) from 30th June, 2018,
  some of the stuff there is still relevant.

* [reddit.com/r/haskell/comments/ai3326/vimneovim\_with\_haskell\_in\_2019\_is\_there\_any
  ](https://www.reddit.com/r/haskell/comments/ai3326/vimneovim_with_haskell_in_2019_is_there_any/)
  from 20th January, 2019.


## Plugins to look into

* [github.com/stefandtw/quickfix-reflector.vim
  ](https://github.com/stefandtw/quickfix-reflector.vim) - Edit entries in
  quickfix window and have them reflected in the buffer.

* [github.com/terryma/vim-multiple-cursors
  ](https://github.com/terryma/vim-multiple-cursors)

* [github.com/ujihisa/unite-haskellimport
  ](https://github.com/ujihisa/unite-haskellimport)

* [github.com/tpope/vim-surround](https://github.com/tpope/vim-surround)

* [github.com/kien/rainbow\_parentheses.vim
  ](https://github.com/kien/rainbow_parentheses.vim)

* [github.com/jacoborus/tender.vim](https://github.com/jacoborus/tender.vim) -
  Theme

* [github.com/enomsg/vim-haskellConcealPlus
  ](https://github.com/enomsg/vim-haskellConcealPlus)
