  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall

in let
    Schema =
      ./Schema.dhall
      sha256:7744398ab254c2f3ae7034a56d8f5f616fed1f7569b7cebeb6fef56bbe055838

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
          , workingDirectory = [] : Optional Text
          } : CommandWrapper.ExecCommand

    , noProfile = [] : Optional Text
    , profile = λ(profile : Text) → [profile] : Optional Text
    }
