let CommandWrapper = ../../command-wrapper/Types.dhall

--let commandWrapper = ../../command-wrapper/library.dhall

in  [
--    { name = "echo"
--    , description = None Text
--    , command =
--          λ(verbosity : CommandWrapper.Verbosity)
--        → λ(colourOutput : CommandWrapper.ColourOutput)
--        → λ(arguments : List Text)
--        → { command = "echo"
--          , arguments = arguments
--          , environment = [] : List CommandWrapper.EnvironmentVariable
--          , searchPath = True
--          , workingDirectory = None Text
--          } : CommandWrapper.ExecCommand
--    , completion =
--        None
--        (   CommandWrapper.Shell
--          → Natural
--          → List Text
--          → CommandWrapper.ExecCommand
--        )
--    } : CommandWrapper.ExecNamedCommand

    ] : List CommandWrapper.ExecNamedCommand
