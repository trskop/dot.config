setlocal number
setlocal colorcolumn=80

augroup filetype_nix
  autocmd! * <buffer>

  " Long or badly placed multi-line strings cause syntax highlighting to fail.
  autocmd BufEnter <buffer> syntax sync fromstart
augroup END
