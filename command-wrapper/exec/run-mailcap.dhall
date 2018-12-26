let CommandWrapper = ../lib/Types.dhall

let Action =
      < View : {}
      | Cat : {}
      | Compose : {}
      | Composetyped : {}
      | Edit : {}
      | Print : {}
      >

let actionToOption : ∀(action : Action) → Text =
        λ(action : Action)
      → merge
          { View = λ(_ : {}) → "--action=view"
          , Cat = λ(_ : {}) → "--action=cat"
          , Compose = λ(_ : {}) → "--action=compose"
          , Composetyped = λ(_ : {}) → "--action=composetyped"
          , Edit = λ(_ : {}) → "--action=edit"
          , Print = λ(_ : {}) → "--action=print"
          }
          action

in    λ(action : Action)
    → λ(environment : List CommandWrapper.EnvironmentVariable)
    → λ(verbosity : CommandWrapper.Verbosity)
    → λ(colourOutput : CommandWrapper.ColourOutput)
    → λ(arguments : List Text)
    → { command = "run-mailcap"
      , arguments =
          [ actionToOption action
          ] # arguments
      , environment = environment
      , searchPath = True
      , workingDirectory = None Text
      } : CommandWrapper.ExecCommand
