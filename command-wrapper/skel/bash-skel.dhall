  λ(libFile : Text)
→ λ(subcommandName : Text)
→ ''
  #!/usr/bin/env bash

  set -e

  source '${libFile}'

  function printHelp() {
      cat <<EOF
  ''${COMMAND_WRAPPER_NAME} ''${subcommandName}
  ''${COMMAND_WRAPPER_NAME} ''${subcommandName} {-h|--help}
  EOF
  }

  function declareCfg() {
      local -r configFile="$1"; shift
      local -r name="$1"; shift

      dhall-to-bash --declare "''${name}" < "''${configFile}"
  }

  function main() {
      dieIfExecutedOutsideOfCommandWrapperEnvironment

      local arg
      while (( $# )); do
          arg="$1"; shift
          case "''${arg}" in
              -h|--help)
                  printHelp
                  exit 0
                  ;;
              # TODO: Define options.
              -*)
                  die 1 "'%s': %s" "''${arg}" 'Unknown option.'
                  ;;
              # TODO: Define arguments.
              *)
                  die 1 "'%s': %s" "''${arg}" 'Too many arguments.'
                  ;;
          esac
      done

      if [[ ! -e "''${COMMAND_WRAPPER_CONFIG}" ]]; then
          info "'%s': Generating default configuration file." \
              "''${COMMAND_WRAPPER_CONFIG}"

          # TODO: Define defaults.
          cat > "''${COMMAND_WRAPPER_CONFIG}" <<< '{=}'
      fi

      info "'%s': Loading configuration file." "''${COMMAND_WRAPPER_CONFIG}"
      eval "$(declareCfg "''${COMMAND_WRAPPER_CONFIG}" 'config')"
      info "'%s': Configuration file loaded." "''${COMMAND_WRAPPER_CONFIG}"

      # TODO: Implement me!
  }

  main "$@"
  ''
