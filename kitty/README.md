Kitty is a terminal emulator designed for power keyboard users.  For more
information about its features see [Kitty homepage
](https://sw.kovidgoyal.net/kitty/).

Configuration:

*   [`kitty.conf`](./kitty.conf) is the main configuration file.

*   [`shortcuts.conf`](./shortcuts.conf) contains custom key mappings.  It has
    to be included by `kitty.conf` for it to apply.

*   [`kitty-local.conf`](./kitty-local.conf) custom settings specific to a
    certain machine (not committed into repository).  It has to be included by
    `kitty.conf` for it to apply.  Kitty ignores non-existing imports,
    therefore, it is safe not to provide this file.

*   `~/.local/src/localhost/dot.config/kitty/kitty-local.conf` custom settings
    specific to a certain machine, but version controlled in a different
    repository.  It has to be included by `kitty.conf` for it to apply.  Kitty
    ignores non-existing imports, therefore, it is safe not to provide this
    file.

Documentation:

*   [`dot.config.kitty(7)`](../man/dot.config.kitty.7.md), it also references
    relevant on-line Kitty documentation in a more focused manner.

*   Kitty documentation is available on its homepage
    [sw.kovidgoyal.net/kitty/](https://sw.kovidgoyal.net/kitty/).
