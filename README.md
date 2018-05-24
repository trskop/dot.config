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
