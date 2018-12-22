{ installScript =
      λ(name : Text)    -- "${toolsetName}_${subcommandName}"
    → λ(toolset : Text)
    → { bash =
          ''
          function _${name}() {
              eval "$('${toolset}' -v env --run-hook="$$")";
          };
          [[ "''${PROMPT_COMMAND}" =~ '_${name}' ]] ||
              PROMPT_COMMAND="_${name}''${PROMPT_COMMAND:+;}''${PROMPT_COMMAND}";
          function _${name}_on_exit() {
              '${toolset}' env --cleanup >/dev/null;
          };
          function _${name}_register_on_exit() {
              eval "local -a x=($(trap -p EXIT))";
              [[ "''${x[2]}" =~ '_${name}' ]] ||
                  trap -- "_${name}_on_exit''${x[2]:+;}''${x[2]}" EXIT;
          };
          _${name}_register_on_exit
          ''
      , fish =
          ''
          # TODO
          ''
      , tcsh =
          ''
          # TODO
          ''
      , zsh =
          ''
          # TODO
          ''
      }

, envFileName =
    -- When 'None' we default to ".${toolset}-${subcommand}".
    None (∀(toolset : Text) → ∀(subcommand : Text) → Text)

, initEnv =
    ''
    empty : Env

    -- vim:ft=dhall
    ''
}
