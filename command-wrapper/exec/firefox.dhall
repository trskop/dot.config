let CommandWrapper = ../lib/Types.dhall

let commandWrapper = ../lib/lib.dhall

let Schema = ./Schema.dhall

in  { invoke =
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
