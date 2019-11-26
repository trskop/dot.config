Kitty is a terminal emulator designed for power keyboard users.  For more
information about its features see its [homepage
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

*   All the documentation is linked from its homepage
    <https://sw.kovidgoyal.net/kitty/>
*   Documentation for `kitty.conf` is available online on
    <https://sw.kovidgoyal.net/kitty/conf.html>.
*   How to control Kitty from command line is described on
    <https://sw.kovidgoyal.net/kitty/remote-control.html>.
*   Key names for shortcut definitions can be found on
    <http://www.glfw.org/docs/latest/group__keys.html>.
*   Key modifier names for shortcut definitions can be found on
    <http://www.glfw.org/docs/latest/group__mods.html>.
