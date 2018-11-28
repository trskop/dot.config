let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:95094b3603fce0a6374a216904826d5b68168414d117de4fe3786673f38e3c6c

let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:6a3233bf9edea9300226f8842a20152288cd37f4deb53128378352487169a639

let
    Schema = ./Schema.dhall

in
    { invoke =
          λ(url : Text)
        → λ(profile : Optional Text)
        → λ(environment : List CommandWrapper.EnvironmentVariable)
        → λ(verbosity : CommandWrapper.Verbosity)
        → λ(colourOutput : CommandWrapper.ColourOutput)
        → λ(arguments : List Text)
        → { command = "firefox"
          -- https://developer.mozilla.org/en-US/docs/Mozilla/Command_Line_Options
          , arguments =
                Optional/fold Text profile (List Text)
                  (λ(p : Text) → ["-P", p])
                  ([] : List Text)
              # [url]
              # arguments
          , environment = environment
          , searchPath = True
          , workingDirectory = None Text
          } : CommandWrapper.ExecCommand

    , noProfile = None Text
    , profile = λ(profile : Text) → Some profile
    }
