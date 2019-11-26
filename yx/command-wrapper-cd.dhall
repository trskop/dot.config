let CommandWrapper = ../command-wrapper/library.dhall

let global = ../command-wrapper/command-wrapper-cd.dhall

let emptyDirectories = CommandWrapper.CdConfig.emptyDirectories

in    global
    ⫽ { directories =
            global.directories
          # (./cd/directories-common.dhall ? emptyDirectories)
          # (./cd/directories-local.dhall ? emptyDirectories)
          # (./cd/directories.dhall ? emptyDirectories)
          # (   ~/.local/src/localhost/this/dot.config/yx/cd/directories-local.dhall
              ? emptyDirectories
            )
      }
