Ripgrep (`rg`) is an alternative implementation of `grep` that is by default
recursive.  It also supports stuff like `.gitignore` to provide better results.

Configuration file [`ripgreprc`](./ripgreprc) is not included automatically; it
has to be passed via `RIPGREP_CONFIG_PATH`.  Easiest way to do this is to
create an alias:

```
alias rg='RIPGREP_CONFIG_PATH="${XDG_CONFIG_HOME:-${HOME}/.config}/ripgrep/ripgreprc" rg'
```

File format is documented in [`rg(1)`
](https://github.com/BurntSushi/ripgrep/blob/master/doc/rg.1.txt.tpl).
