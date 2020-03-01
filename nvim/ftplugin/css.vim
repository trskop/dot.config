setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal colorcolumn=80
setlocal number

augroup filetype_css
  autocmd! * <buffer>
  autocmd BufWritePre <buffer> %s/\s\+$//e
augroup END
