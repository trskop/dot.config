MPlayer doesn't support XDG Base Directory Specification. To get around this
we have few options:

Create a symbolic link to `~/.config/mplayer`:

```Bash
cd ~
ln -s .config/mplayer .mplayer
```

We can use shell alias which specifies configuration file:

```Bash
alias 'MPLAYER_HOME="${XDG_CONFIG_HOME:${HOME}/.config}/mplayer" mplayer'
```

Lastly we can just export `MPLAYER_HOME` environment variable in our shell
rc-file:

```
export MPLAYER_HOME="${XDG_CONFIG_HOME:${HOME}/.config}/mplayer"
```

Reading material:

* [ArchWiki — XDG Base Directory
  ](https://wiki.archlinux.org/title/XDG_Base_Directory)
