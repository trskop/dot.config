GHCi doesn't support XDG Base Directory Specification, see
[GHC User's Guide: The .ghci and .haskeline files
](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-and-haskeline-files)
for more information.

To work around this issue we are using symlink:

```Bash
cd ~
ln -s .config/ghc .ghc
```

Reading material:

* [Haskell Wiki: GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
