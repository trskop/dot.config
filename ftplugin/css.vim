setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal colorcolumn=80
setlocal number
autocmd BufWritePre * %s/\s\+$//e
