% DOT-CONFIG-KITTY(7) User's Kitty Config | User's Dot Files
% Peter Trsko
% 18th July 2021

# NAME

dot-config-kitty - Documentation of user's Kitty terminal emulator
configuration.


# DESCRIPTION

Kitty is a terminal emulator designed for power keyboard users.  For more
information about its features see [Kitty homepage
](https://sw.kovidgoyal.net/kitty/).

This manual page focuses on specifics that are part of [dot.config Git
repository](https://github.com/trskop/dot.config). For more general
documentation see [Kitty documentation — Configuring kitty
](https://sw.kovidgoyal.net/kitty/conf/).


# SHORTCUTS

These are configured in `shortcuts.conf`.

See also:

* `${XDG_CONFIG_HOME:-${HOME}/.config}/kitty/shortcuts.conf` in **DIRECTORIES
  AND FILES** section of this manual page.
* [Kitty documentation — Mappable actions
  ](https://sw.kovidgoyal.net/kitty/actions/)
* Key names for shortcut definitions can be found in GLFW documentation on
  [`glfw.org/docs/latest/group__keys.html`
  ](http://www.glfw.org/docs/latest/group__keys.html).
* Key modifier names for shortcut definitions can be found in GLFW
  documentation on [`glfw.org/docs/latest/group__mods.html`
  ](http://www.glfw.org/docs/latest/group__mods.html).

## SCROLLING

`SHIFT+PAGE_UP`, `SHIFT+PAGE_DOWN`
:   Scroll one page up or down, respectively.

`CTRL+SHIFT+k`, `CTRL+SHIFT+up`
:   Scroll one line up, like in Vim. `SHIFT` added to allow shell to use Vim
    bindings.The `CTRL+SHIFT+up` is cumbersome to use on ErgoDox keyboard, but
    it's still very useful on standard keyboards, especially on laptops.

`CTRL+SHIFT+j`, `CTRL+SHIFT+down`
:   Scroll one line down, like in Vim.`SHIFT` added to allow shell to use Vim
    bindings. The `CTRL+SHIFT+down` is cumbersome to use on ErgoDox keyboard,
    but it's still very useful on standard keyboards, especially on laptops.

`CTRL+SHIFT+HOME`, `CTRL+SHIFT+END`
:   Scroll to top or bottom, respectively.

    **TODO**: Find a good alternative that works on ErgoDox layout that doesn't
    have `HOME` and `END` key on the first layer.

## CLIPBOARD

`CTRL+SHIFT+v`
:   Paste from X clipboard buffer (not the same as selection). The `SHIFT` bit
    in the shortcut is there for consistency with `CTRL+SHIFT+c`, which was
    selected to avoid remapping `CTRL+c` to a non-standard use.

`CTRL+SHIFT+c`, `SHIFT+INSERT`
:   Copy selection to X clipboard buffer. The `SHIFT` part of `CTRL+SHIFT+c`
    shortcut was picked to avoid remapping `CTRL+c` to a non-standard use, but
    to stay close to standard copy/paste shortcut.

## TABS

`CTRL+SHIFT+t`
:   Open a new terminal tab.

`CTRL+SHIFT+q`
:   Close current terminal tab.

`CTRL+SHIFT+RIGHT`
:   Move to the next terminal tab.

`CTRL+SHIFT+LEFT`
:   Move to the previous terminal tab.

`CTRL+SHIFT+SPACE`
:   Switch between terminal window layouts. Shortcut selected to be similar
    with user's XMonad settings.

## WINDOWS

`CTRL+SHIFT+ENTER`
:   New terminal window (not OS window).

`CTRL+SHIFT+n`
:   New OS terminal window.

`CTRL+SHIFT+h`
:   Focus/select previous terminal window (not OS window), like in Vim. `SHIFT`
    added to allow shell to use Vim bindings.

`CTRL+SHIFT+l`, `CTRL+SHIFT+TAB`
:   Focus/select next terminal window, like in Vim. `SHIFT` added to allow
    shell to use Vim bindings.

    `CTRL+SHIFT+TAB` shortcut was introduced for consistency with some other
    tools user is using. Makes it less confusing when switching back and forth.

`CTRL+SHIFT+0`, `CTRL+SHIFT+1`, `CTRL+SHIFT+2`, ..., and `CTRL+SHIFT+9`
:   Focus first (index `0`), second (index `1`), third (index `2`), ..., tenth
    (index `9`) window, respectively.

## MISCELLANEOUS

`CTRL+SHIFT+EQUAL`, `CTRL+SHIFT+PLUS`
:   Increase font size. Using both `EQUAL` (`=`) and `PLUS` (`+`) keys allows
    for better transition between various keyboard layouts. Especially the
    `SHIFT` part of the shortcut can be confusing as difference between `=` and
    `+` on many layouts is `SHIFT`.

    Dual to `CTRL+SHIFT+MINUS`.

`CTRL+SHIFT+MINUS`
:   Decrease font size.

    Dual to `CTRL+SHIFT+EQUAL` and `CTRL+SHIFT+PLUS`.

`CTRL+SHIFT+BACKSPACE`
:   Restore font size to default. The default itself is set in `kitty.conf`.

`CTRL+SHIFT+F11`
:   Toggle fullscreen.

    **TODO**: Considering using a different shortcut for this as it is unusable
    on keyboards without F-row.

`CTRL+SHIFT+F10`
:   Toggle maximised.

    **TODO**: Considering using a different shortcut for this as it is unusable
    on keyboards without F-row.

`CTRL+SHIFT+u`
:   Input Unicode character.

`CTRL+SHIFT+ESCAPE`
:   Run the kitty shell in a terminal window (not OS window) to control kitty
    with commands.

`CTRL+SHIFT+DELETE`
:   Clear the terminal.

## DISABLED SHORTCUTS

Some shortcuts that are part of the default Kitty configuration were explicitly
disabled (set to `no_op`):

* `CTRL+SHIFT+w` — Originally intended to close terminal OS window. Caused a
  lot of accidental window closures.

* `CTRL+SHIFT+[`, `CTRL+SHIFT+]` — Originally movement between windows. Quite
  unusable on non-standard keyboard layouts, confusing when using similar
  shortcuts on other applications with
  vastly different meaning.

  Use `CTRL+SHIFT+h` and `CTRL+SHIFT+l` instead, respectively.

* `CTRL+SHIFT+f`, `CTRL+SHIFT+b`, and `CTRL+SHIFT+\`` — Originally moved window
  forward, backward, and on top. Wasn't able to remember these.

  Use Kitty shell via `CTRL+SHIFT+ESCAPE` to perform these kind of actions
  instead.

* `CTRL+SHIFT+q` — Originally closed current tab. This was a dangerously close
  to `CTRL+q` for me.

  Use Kitty shell via `CTRL+SHIFT+ESCAPE` to perform this if it's not possible
  to simply terminate the application running in that specific tab.

* `CTRL+SHIFT+,` and `CTRL+SHIFT+.` — Originally moved active tab
  forward/backward, respectively. These turned out to be completely
  unintuitive.

* `CTRL+SHIFT+ALT+t` — Originally meant to set tab title. Wasn't able to
  remember it at all.

  Use Kitty shell via `CTRL+SHIFT+ESCAPE` to perform this action instead.

* `CTRL+SHIFT+a>1`, `CTRL+SHIFT+a>d`, `CTRL+SHIFT+a>l`, and `CTRL+SHIFT+a>m` —
  Originally intended to set background opacity. Never used it.

  Use Kitty shell via `CTRL+SHIFT+ESCAPE` to perform these kind of actions
  instead.

* `CTRL+SHIFT+F2`, `CTRL+SHIFT+F5`, and `CTRL+SHIFT+F6` — Originally used for
  Edit, reload, and debug `kitty.conf`, respectively. Never used them.

  Use Kitty shell via `CTRL+SHIFT+ESCAPE` to perform these kind of actions
  instead.

* `CTRL+SHIFT+o` — Originally sent current selection to an application. It was
  basically unusable to me as it relied on detecting URI schemes by default. It
  is possible to set what application should be targeted, but that is better to
  be done ad-hoc.

See also standard shortcuts documented on

# DIRECTORIES AND FILES

`${XDG_CONFIG_HOME:-${HOME}/.config}/kitty/`
:   Configuration directory mandated by XDG Base Directory Specification which
    is used for managed user's dot files.

    See also:

    * [Kitty documentation — Configuring kitty](https://sw.kovidgoyal.net/kitty/conf/).
    * Manual page `file-hierarchy(7)` section HOME DIRECTORY regarding
      `${XDG_CONFIG_HOME:-${HOME}/.config}` directory.

`${XDG_CONFIG_HOME:-${HOME}/.config}/kitty/kitty.conf`
:   Main configuration file of Kitty terminal emulator.

    See also:

    * [Kitty documentation — Configuring kitty](https://sw.kovidgoyal.net/kitty/conf/).
    * Manual page `file-hierarchy(7)` section HOME DIRECTORY regarding
      `${XDG_CONFIG_HOME:-${HOME}/.config}` directory.

`${XDG_CONFIG_HOME:-${HOME}/.config}/kitty/shortcuts.conf`
:   User's custom key mappings. This configuration file imported by:

    ```
    ${XDG_CONFIG_HOME:-${HOME}/.config}/kitty/kitty.conf
    ```

    It would not be applied without it being imported.

    See also:

    * [Kitty documentation — Configuring kitty](https://sw.kovidgoyal.net/kitty/conf/).
    * [Kitty documentation — Configuring kitty — Keyboard shortcuts](https://sw.kovidgoyal.net/kitty/conf/#keyboard-shortcuts)
    * Manual page `file-hierarchy(7)` section HOME DIRECTORY regarding
      `${XDG_CONFIG_HOME:-${HOME}/.config}` directory.

`${XDG_CONFIG_HOME:-${HOME}/.config}/kitty/kitty-local.conf`
:   User's custom settings specific to this machine that are not version
    controlled. This file is not committed into the repository and is useful
    for trying new stuff and other temporary configuration.

    Permanent machine specific settings should go into:

    ```
    ${HOME}/.local/src/localhost/dot.config/kitty/kitty-local.conf
    ```

    Which should also be version controlled.

    File `kitty-local.conf` has to be included by `kitty.conf` for it to apply.
    Kitty ignores non-existing imports, therefore, it is safe not to provide
    this file.

    See also:

    * [Kitty documentation — Configuring kitty](https://sw.kovidgoyal.net/kitty/conf/).
    * [Kitty documentation — Configuring kitty — Keyboard shortcuts](https://sw.kovidgoyal.net/kitty/conf/#keyboard-shortcuts)
    * Manual page `file-hierarchy(7)` section HOME DIRECTORY regarding
      `${XDG_CONFIG_HOME:-${HOME}/.config}` directory.

`${HOME}/.local/src/localhost/dot.config/kitty/kitty-local.conf`
:   User's custom settings specific to this machine that are version
    controlled. This file is committed into a separate repository that is
    machine specific and should never be made public.

    Temporary settings that do not need to be version controlled can be put
    into:

    ```
    ${XDG_CONFIG_HOME:-${HOME}/.config}/kitty/kitty-local.conf
    ```

    File `kitty-local.conf` has to be included by `kitty.conf` for it to apply.
    Kitty ignores non-existing imports, therefore, it is safe not to provide
    this file.

    See also:

    * [Kitty documentation — Configuring kitty](https://sw.kovidgoyal.net/kitty/conf/).
    * [Kitty documentation — Configuring kitty — Keyboard shortcuts](https://sw.kovidgoyal.net/kitty/conf/#keyboard-shortcuts)
    * Manual page `file-hierarchy(7)` section HOME DIRECTORY regarding
      `${XDG_CONFIG_HOME:-${HOME}/.config}` directory.


# SEE ALSO

kitty(1),
dot.gitconfig(7),
file-hierarchy(7)

* [Kitty homepage](https://sw.kovidgoyal.net/kitty/)
* [Kitty documentation — Configuring kitty
  ](https://sw.kovidgoyal.net/kitty/conf/).
* [Kitty documentation — Configuring kitty — Keyboard shortcuts
  ](https://sw.kovidgoyal.net/kitty/conf/#keyboard-shortcuts)
* [Kitty documentation — Mappable actions
  ](https://sw.kovidgoyal.net/kitty/actions/)
* Key names for shortcut definitions can be found in GLFW documentation on
  [`glfw.org/docs/latest/group__keys.html`
  ](http://www.glfw.org/docs/latest/group__keys.html).
* Key modifier names for shortcut definitions can be found in GLFW
  documentation on [`glfw.org/docs/latest/group__mods.html`
  ](http://www.glfw.org/docs/latest/group__mods.html).


# BUGS

<https://github.com/trskop/dot.config/issues>
