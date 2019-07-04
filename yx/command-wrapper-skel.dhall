let mkGlobal = ../command-wrapper/command-wrapper-skel.dhall

in    λ(toolset : Text)
    → λ(subcommand : Text)
    → λ(command : Text)
    →   mkGlobal toolset subcommand command
      //  {=}
