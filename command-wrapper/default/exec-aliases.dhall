let CommandWrapper = ~/.config/command-wrapper/Types.dhall

let execConfig = ../command-wrapper-exec.dhall

let List/map =
      https://prelude.dhall-lang.org/List/map
      sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let toAlias =
        λ(x : CommandWrapper.ExecNamedCommand)
      → { alias = x.name, command = "exec", arguments = [ x.name ] }

in  List/map
    CommandWrapper.ExecNamedCommand
    CommandWrapper.SubcommandAlias
    toAlias
    execConfig.commands
