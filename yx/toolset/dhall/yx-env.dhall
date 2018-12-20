{ installScript =
      λ(name : Text)
    → λ(toolset : Text)
    → { bash =
          ''
          function _${name}() {
              eval "$(${toolset} env --run-hook )";
          };
          [[ "''${PROMPT_COMMAND}" =~ '_${name}' ]] ||
              PROMPT_COMMAND="_${name}''${PROMPT_COMMAND:+;}''${PROMPT_COMMAND}";
          ''
      }
}
