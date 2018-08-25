  let
    home = env:HOME as Text

in let
    fzf = "${home}/.config/nvim/dein.vim/repos/github.com/junegunn/fzf/bin/fzf"

in  fzf : Text
