-- vim: filetype=dhall
--
-- To bump Command Wrapper libraries just change that SHA1 of the commit, and
-- run `habit config --dhall-hash <<< $URL` to get integrity hash.  In
-- Vim/Neovim this can be done by copying the URL to another line, and then
-- running following Ex command on it:
--
-- ```
-- :.!yx config --dhall-hash
-- ```
--
-- Other option is to run `habit config --dhall-freeze` on the whole file.  In
-- Vim/Neovim we can do this using:
--
-- ```
-- :%!yx config --dhall-freeze
-- ```
--
-- To use latest (known) version of the library we can ask Command Wrapper
-- itself:
-- 
-- ```
-- :.!yx completion --library --dhall=exec --import
-- ```
https://raw.githubusercontent.com/trskop/command-wrapper/0.1.0.0-rc9/command-wrapper/dhall/Exec/package.dhall sha256:2d55aab0714cb4f4042be8605b7f23f330a7a3763b612f243fb6eea32535fdd5
