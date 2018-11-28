let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:95094b3603fce0a6374a216904826d5b68168414d117de4fe3786673f38e3c6c

let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:6a3233bf9edea9300226f8842a20152288cd37f4deb53128378352487169a639

let
    context =
      { home = "${env:HOME as Text}"
      }

in  commandWrapper.mkSkelConfig context ./templates.dhall
