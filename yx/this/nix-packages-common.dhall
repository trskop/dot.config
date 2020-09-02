-- A static analysis tool for shell scripts.
-- <https://github.com/koalaman/shellcheck>
[ "shellcheck"

-- Enhanced variant of `cat`. See ../../bat/README.md for more.
, "bat"

-- Compile diagrams from simple human readable text representation.
-- <http://plantuml.com/>
, "plantuml"

-- Universal document converter.  Used for building Command Wrapper's manual
-- pages.  For that reason we need it to be installed system-wide as well as
-- via Nix.
-- <https://pandoc.org/>
, "pandoc"  -- Universal document converter.  Used for building Command

-- Directory specific shell configuration.
-- Homepage: <https://direnv.net/>
-- GitHub: <https://github.com/direnv/direnv>
, "direnv"

-- A tool for defining and running multi-container Docker applications.
-- <https://docs.docker.com/compose/>
, "docker-compose"  -- A tool for defining and running multi-container Docker

-- Control Neovim process from shell.
-- <https://github.com/mhinz/neovim-remote>
, "neovim-remote"

, "youtube-dl"

-- Simple, fast and user-friendly alternative to find.
-- <https://github.com/sharkdp/fd>
, "fd"

-- Full-featured alternative to `fzf` written in Rust.
-- <https://github.com/lotabout/skim>
, "skim"

-- Formatter for Nix code.
-- <https://github.com/serokell/nixfmt>
, "nixfmt"

-- Allows you to change layout and styling of diffs, as well as allowing you to
-- stay arbitrarily close to the default git/diff output.
-- <https://github.com/dandavison/delta>
--
-- See also `${XDG_CONFIG_HOME:-${HOME}/.config}/git/delta.gitconfig` as it is
-- used by Git.
, "delta"
]
