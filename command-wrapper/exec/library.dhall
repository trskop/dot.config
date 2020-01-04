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
https://raw.githubusercontent.com/trskop/command-wrapper/5fdf733e3202aa28c251c90b191d462e60719a60/dhall/Exec/package.dhall sha256:ee2a4c35889b685ee46a10ce1516e2037091839d3ebd53c0b8a8c4a128bf517a
