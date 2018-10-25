  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:08d2673948c732c338f5322ee7c15a3f4b92c27dce731d7c678026eb9309efe6

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:ca555d6f0c8621b29bb8b7fc7566c2617efa634b5ec1e909cd10346b0ad15faf

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
