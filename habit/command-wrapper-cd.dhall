let global = ../command-wrapper/command-wrapper-cd.dhall

let directories-local = ./cd/directories-local ? ([] : List Text)

let directories-common = ./cd/directories-common.dhall ? ([] : List Text)

let directories = ./cd/directories.dhall ? ([] : List Text)

in      global
    //  { directories =
              global.directories
            # directories-common
            # directories
            # directories-local
        }
