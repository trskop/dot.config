  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    optional = Optional/fold

in let
    verbosityOption = commandWrapper.verbosity.fold (List Text)
      { Silent = λ(_ : {}) → ["--silent"]
      , Normal = λ(_ : {}) → [] : List Text
      , Verbose = λ(_ : {}) → ["--verbosity=info"]
      , Annoying = λ(_ : {}) → ["--verbosity=debug"]
      }

in let
    colorOption =
        λ(colourOutput : CommandWrapper.ColourOutput)
      → ["--color=${commandWrapper.colourOutput.toText colourOutput}"]

in let
    stackYamlOption =
        λ(stackYaml : Optional Text)
      → optional Text stackYaml
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
      }
