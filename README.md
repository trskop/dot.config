# ~/.config/nvim

Personalised Neovim configuration.

## Installation

### Linux

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


### All Platforms

Install Haskell IDE Engine

```Bash
git clone https://github.com/haskell/haskell-ide-engine ~/.local/src/haskell/haskell-ide-engine
cd ~/.local/src/haskell/haskell-ide-engine
stack install
```

Don't forget that `~/.local/bin` has to be in your `PATH`.

Configure Neovim:

```Bash
git clone git@github.com:trskop/dot.config.nvim.git ~/.config/nvim
cd ~/.config/nvim
wget https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh
# Review installer.sh before executing it.
sh installer.sh dein.vim
nvim -c ':UpdateRemotePlugins'
```
