Tmux doesn't support XDG directories see their [GitHub issue #142
](https://github.com/tmux/tmux/issues/142) for more details. To get around this
we can either create a symlink:

```Bash
cd ~
ln -s .config/tmux/tmux.conf .tmux.conf
```

Or we can use shell alias which specifies configuration file:

```Bash
alias tmux='TERM=xterm-256color tmux -f "${XDG_CONFIG_HOME}"/tmux/tmux.conf'
```

Reading material:

* [tmux-sensible](zhttps://github.com/tmux-plugins/tmux-sensible)
