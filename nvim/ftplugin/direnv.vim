setlocal softtabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal colorcolumn=80
setlocal number

augroup filetype_direnv
  autocmd! * <buffer>

  autocmd BufWritePre <buffer> %s/\s\+$//e

  " Long or badly placed multi-line strings cause syntax highlighting to fail.
  autocmd BufEnter <buffer> syntax sync fromstart
augroup END

" Neomake is integrated with ShellCheck, this will enable it:
call neomake#configure#automake('nrwi', 500)
