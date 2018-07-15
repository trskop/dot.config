Haskeline is a readline alternative written in Haskell and used by GHCi. At the
moment it doesn't support XDG standard, which can be seen in
[System.Console.Haskeline.InputT module
](https://github.com/judah/haskeline/blob/3bf2b621685fc795f0dc3f3196728ccea492d42b/System/Console/Haskeline/InputT.hs#L199)

To work around this issue we are using symlink:

```Bash
cd ~
ln -s .config/haskeline/prefs .haskeline
```
