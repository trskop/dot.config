setlocal softtabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal colorcolumn=80
setlocal number

setlocal makeprg=plantuml\ %

augroup filetype_plantuml
  autocmd! * <buffer>

  autocmd BufWritePre <buffer> %s/\s\+$//e
augroup END
