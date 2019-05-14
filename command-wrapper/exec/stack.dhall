let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

let verbosityOption = commandWrapper.verbosity.fold (List Text)
      { Silent = ["--silent"]
      , Normal = [] : List Text
      , Verbose = ["--verbosity=info"]
      , Annoying = ["--verbosity=debug"]
      }

let colorOption =
        λ(colourOutput : CommandWrapper.ColourOutput)
      → ["--color=${commandWrapper.colourOutput.toText colourOutput}"]

let stackYamlOption =
        λ(stackYaml : Optional Text)
      → Optional/fold Text stackYaml
          (List Text)
          (λ(file : Text) → ["--stack-yaml=${file}"])
          ([] : List Text)

in    λ ( context
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
