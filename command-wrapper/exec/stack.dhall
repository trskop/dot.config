let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:95094b3603fce0a6374a216904826d5b68168414d117de4fe3786673f38e3c6c

let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:6a3233bf9edea9300226f8842a20152288cd37f4deb53128378352487169a639

let
    verbosityOption = commandWrapper.verbosity.fold (List Text)
      { Silent = λ(_ : {}) → ["--silent"]
      , Normal = λ(_ : {}) → [] : List Text
      , Verbose = λ(_ : {}) → ["--verbosity=info"]
      , Annoying = λ(_ : {}) → ["--verbosity=debug"]
      }

let
    colorOption =
        λ(colourOutput : CommandWrapper.ColourOutput)
      → ["--color=${commandWrapper.colourOutput.toText colourOutput}"]

let
    stackYamlOption =
        λ(stackYaml : Optional Text)
      → Optional/fold Text stackYaml
          (List Text)
          (λ(file : Text) → ["--stack-yaml=${file}"])
          ([] : List Text)

in
      λ ( context
            : { workingDirectory : Optional Text
              , stackYaml : Optional Text
              }
        )
    → λ(args : List Text)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(colourOutput : CommandWrapper.ColourOutput)
    → λ(arguments : List Text)
    → { command = "stack"
      , arguments =
            verbosityOption verbosity
          # colorOption colourOutput
          # stackYamlOption context.stackYaml
          # args
          # arguments
      , environment = [] : List CommandWrapper.EnvironmentVariable
      , searchPath = True
      , workingDirectory = context.workingDirectory
      } : CommandWrapper.ExecCommand
