setlocal softtabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal colorcolumn=80
setlocal number
autocmd BufWritePre * %s/\s\+$//e
