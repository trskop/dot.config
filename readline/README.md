Readline library doesn't support XDG standard. See e.g. [ArchWiki: XDG Base
Directory](https://wiki.archlinux.org/index.php/XDG_Base_Directory) for more
information.

To get around this issue we can use following:

```Bash
cd ~
ln -s .config/readline/inputrc .inputrc
```

Or we can use `INPUTRC` environment variable:

```Bash
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc
```
