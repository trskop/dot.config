CommandWrapper configuration, see [github.com/trskop/command-wrapper
](https://github.com/trskop/command-wrapper) for more information.

Build Configuration:

```Bash
./generate.hs
```

The above is not completely necessary, but imports are causing unnecessary
delay, and they may stop working when internet is not available. This way we
get minimal configuration with all dependencies resolved.
