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
    -- When 'None Text' we default to ".${toolset}-${subcommand}".
    None (∀(toolset : Text) → ∀(subcommand : Text) → Text)

, initEnv = ''
-- Following definitions are automatically in scope:
--
-- let Variable : Type = {name : Text, value : Text}
--
-- Following data type is just a placeholder for future implementation:
-- let Action : Type =
--       { name : Text
--       , action : Text
--       }
--
-- let Actions : Type = List Action
--
-- let VariableOperation : Type =
--       < Set : {name : Text, value : Text}  -- Same as 'Variable'.
--       | Unset : {name : Text}
--       | Modify : {name : Text, modify : Optional Text → Optional Text}
--       >
--
-- let VariableOperations : Type = List VariableOperation
--
-- let Env : Type =
--       { variables : Text → VariableOperations
--       , actions : Actions
--       }
--
-- let empty : Env =
--       { variables = λ(_ : Text) → [] : VariableOperations
--       , actions = List Actions
--       }
--
empty : Env

-- Example:
--
-- { variables =
--       λ(dir : Text)
--     → [ VariableOperation.Modify
--           { name = "PATH"
--           , modify =
--                 λ(value : Optional Text)
--               → Optional/fold Text value (Optional Text)
--                   (λ(path : Text) → Some "''${dir}/bin:''${path}")
--                   (Some "''${dir}")
--           }
--       ] : VariableOperations
--
-- , actions = empty.actions
-- }

-- vim:ft=dhall
''
}
