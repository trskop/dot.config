let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

let verbosityOptions = commandWrapper.verbosity.fold (List Text)
      { Silent = λ(_ : {}) → ["--logging=0"]
      , Normal = λ(_ : {}) → [] : List Text
      , Verbose = λ(_ : {}) → ["--logging=5", "--explain"]
      , Annoying = λ(_ : {}) →
          [ "--logging=6"
          , "--explain"
          , "--verbose_explanations"
          , "--verbose_failures"
          ]
      }

let colorOption =
        λ(colourOutput : CommandWrapper.ColourOutput)
      → "--color=${commandWrapper.colourOutput.toText colourOutput}"

in    λ(workingDirectory : Optional Text)
    → λ(args : List Text)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(colourOutput : CommandWrapper.ColourOutput)
    → λ(extraArgs : List Text)
    → { command = "bazel"
      , arguments =
          let arguments = ./head-and-tail.dhall Text (args # extraArgs)
          in    verbosityOptions verbosity
              # Optional/fold Text arguments.head (List Text)
                  (λ(cmd : Text) → [cmd, colorOption colourOutput])
                  ([] : List Text)
              # arguments.tail
      , environment = [] : List CommandWrapper.EnvironmentVariable
      , searchPath = True
      , workingDirectory = workingDirectory
      } : CommandWrapper.ExecCommand
