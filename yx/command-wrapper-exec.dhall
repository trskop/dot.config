let CommandWrapper = ../command-wrapper/library.dhall

let global
    : CommandWrapper.ExecConfig.Type
    = ../command-wrapper/command-wrapper-exec.dhall

let constructor
    : ∀(global : CommandWrapper.ExecConfig.Type) →
        CommandWrapper.ExecConfig.Type
    = ./exec/constructor.dhall

in  constructor global : CommandWrapper.ExecConfig.Type
