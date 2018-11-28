let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:95094b3603fce0a6374a216904826d5b68168414d117de4fe3786673f38e3c6c

in
    { commands =
        ./commands.dhall ? ([] : List CommandWrapper.ExecNamedCommand)
    }
