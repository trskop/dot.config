let CommandWrapper = ../../command-wrapper/library.dhall

let -- Need unreleased version
    Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/8b73fad3d6022e8095375397f6d36419670fd7de/command-wrapper/dhall/Exec/package.dhall sha256:a37898b96a6774e3c56ba9f85686dafb913886c0f1fb1d24cc3578a1ad1893e7

let Prelude =
      https://prelude.dhall-lang.org/v17.0.0/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let -- List of nix commands that we want to provide. This is just a subset of
    -- what's in Exec.nix.Command.Type
    nixCommands =
      [ Exec.nix.Command.Type.nix
      , Exec.nix.Command.Type.nix-build
      , Exec.nix.Command.Type.nix-channel
      , Exec.nix.Command.Type.nix-collect-garbage
      , Exec.nix.Command.Type.nix-copy-closure
      , Exec.nix.Command.Type.nix-env
      , Exec.nix.Command.Type.nix-hash
      , Exec.nix.Command.Type.nix-install-package
      , Exec.nix.Command.Type.nix-instantiate
      , Exec.nix.Command.Type.nix-prefetch-url
      , Exec.nix.Command.Type.nix-push
      , Exec.nix.Command.Type.nix-shell
      , Exec.nix.Command.Type.nix-store
      ]

let nixCommandToExecNamedCommand =
      λ(nix-command : Exec.nix.Command.Type) →
        let name = Exec.nix.Command.show nix-command
        
        in  CommandWrapper.ExecNamedCommand::{
            , name
            , description = Some
                "Just invoke '${name}', but with command-line completion."
            , command = Exec.nix.command nix-command ([] : List Text)
            , completion = Some
                ( Exec.nix.completion
                    toolset
                    nix-command
                    (None Text)
                    ([] : List Text)
                )
            }

in  Prelude.List.map
      Exec.nix.Command.Type
      CommandWrapper.ExecNamedCommand.Type
      nixCommandToExecNamedCommand
      nixCommands
