setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal colorcolumn=80
setlocal number
" We can't use this any more.  Lines with only whitespace are significant in
" case of multiline string literals.  I think it's possible to design a regex
" that would work in presence of those, but it won't be able to detect all
" trailing whitespace.
"autocmd BufWritePre * %s/\s\+$//e
