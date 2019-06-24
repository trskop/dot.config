let CommandWrapper = ../Types.dhall

let List/map =
      https://prelude.dhall-lang.org/List/map
      sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

in    λ(command : Text)
    → λ(prefixArguments : List Text)
    → λ(_ : CommandWrapper.Shell)
    → λ(index : Natural)
    → λ(words : List Text)
    → { command =
          command
      , arguments =
          let adjustedIndex = index + List/length Text prefixArguments
          
          in    [ "--bash-completion-index=${Natural/show adjustedIndex}" ]
              # List/map
                Text
                Text
                (λ(word : Text) → "--bash-completion-word=${word}")
                (prefixArguments # words)
      , environment =
          [] : List CommandWrapper.EnvironmentVariable
      , searchPath =
          True
      , workingDirectory =
          None Text
      }

