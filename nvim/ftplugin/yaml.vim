setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal colorcolumn=80
setlocal number

augroup filetype_yaml
  autocmd! * <buffer>

  autocmd BufWritePre <buffer> %s/\s\+$//e

  " Long or badly placed multi-line strings cause syntax highlighting to fail.
  autocmd BufEnter <buffer> syntax sync fromstart
augroup END
