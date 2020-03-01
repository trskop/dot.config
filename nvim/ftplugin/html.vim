setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal colorcolumn=80

augroup filetype_html
  autocmd! * <buffer>

  autocmd BufWritePre <buffer> %s/\s\+$//e
augroup END
