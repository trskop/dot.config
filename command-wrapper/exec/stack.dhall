  let
    CommandWrapper = ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    optional = Optional/fold

in let
    FromVerbosity =
        ∀(r : Type)
      → ∀ (handler
            : { Silent : ∀(_ : {}) → r
              , Normal : ∀(_ : {}) → r
              , Verbose : ∀(_ : {}) → r
              , Annoying : ∀(_ : {}) → r
              }
          )
      → ∀(verbosity : CommandWrapper.Verbosity)
      → r

in let
    fromVerbosity : FromVerbosity =
        λ(r : Type)
      → λ(handler
            : { Silent : ∀(_ : {}) → r
              , Normal : ∀(_ : {}) → r
              , Verbose : ∀(_ : {}) → r
              , Annoying : ∀(_ : {}) → r
              }
          )
      → λ(verbosity : CommandWrapper.Verbosity)
      → merge handler verbosity

in let
    verbosityOption = fromVerbosity (List Text)
      { Silent = λ(_ : {}) → ["--silent"]
      , Normal = λ(_ : {}) → [] : List Text
      , Verbose = λ(_ : {}) → ["--verbosity=info"]
      , Annoying = λ(_ : {}) → ["--verbosity=debug"]
      }

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
    → λ(arguments : List Text)
    → { command = "stack"
      , arguments =
            verbosityOption verbosity
          # stackYamlOption context.stackYaml
          # args
          # arguments
      , environment = [] : List CommandWrapper.EnvironmentVariable
      , searchPath = True
      , workingDirectory = context.workingDirectory
      }
