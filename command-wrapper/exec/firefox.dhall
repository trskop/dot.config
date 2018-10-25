  let
    CommandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/Type/package.dhall
      sha256:08d2673948c732c338f5322ee7c15a3f4b92c27dce731d7c678026eb9309efe6

in let
    commandWrapper =
      ~/.local/src/trskop/command-wrapper/dhall/CommandWrapper/package.dhall
      sha256:ca555d6f0c8621b29bb8b7fc7566c2617efa634b5ec1e909cd10346b0ad15faf

in let
    Schema =
      ./Schema.dhall
      sha256:6a8978aae0ebeaabbe46517a2fdae360835d1cd37f62c5aea46d86bce2505b63

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
