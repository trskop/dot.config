setlocal softtabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal colorcolumn=80
setlocal number

augroup filetype_purescript
  autocmd! * <buffer>

  autocmd BufWritePre <buffer> %s/\s\+$//e

  " Long or badly placed multi-line strings cause syntax highlighting to fail.
  autocmd BufEnter <buffer> syntax sync fromstart
augroup END
