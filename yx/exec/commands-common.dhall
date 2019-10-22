let CommandWrapper = ../../command-wrapper/library.dhall

in  [
--    { name = "echo"
--    , description = None Text
--    , command =
--          λ(verbosity : CommandWrapper.Verbosity.Type)
--        → λ(colourOutput : CommandWrapper.ColourOutput.Type)
--        → λ(arguments : List Text)
--        → { command = "echo"
--          , arguments = arguments
--          , environment = [] : List CommandWrapper.EnvironmentVariable.Type
--          , searchPath = True
--          , workingDirectory = None Text
--          } : CommandWrapper.ExecCommand.Type
--    , completion =
--        None
--        (   CommandWrapper.Shell.Type
--          → Natural
--          → List Text
--          → CommandWrapper.ExecCommand.Type
--        )
--    } : CommandWrapper.ExecNamedCommand.Type

    ] : List CommandWrapper.ExecNamedCommand.Type
