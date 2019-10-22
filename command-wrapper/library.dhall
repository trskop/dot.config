-- vim: filetype=dhall
--
-- To bump Command Wrapper libraries just change that SHA1 of the commit, and
-- run `habit config --dhall-hash <<< $URL` to get integrity hash.  In
-- Vim/Neovim this can be done by copying the URL to another line, and then
-- running following Ex command on it:
--
-- ```
-- :.!habit config --dhall-hash
-- ```
--
-- Other option is to run `habit config --dhall-freeze` on the whole file.  In
-- Vim/Neovim we can do this using:
--
-- ```
-- :%!habit config --dhall-freeze
-- ```
https://raw.githubusercontent.com/trskop/command-wrapper/635d48aaa3864db4ecc0ddd3096243b4b5ebed4e/dhall/CommandWrapper/package.dhall sha256:60dc62ca6d93b6d9b8e76d7aca5b7c0a8c2dca137777e96dfd5f6ae2d1e15fdb
