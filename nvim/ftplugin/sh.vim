" ShellCheck is static analysis tool for shell.  In case of libraries we may
" want to avoid shebang, but we may still provide ShellCheck directive for it
" to use the correct shell.  If that is present we can set proper syntax
" highlighting based on it.
if getline(1) =~ '^#\s*shellcheck\s\+shell=bash'
  let b:is_bash = 1
endif

setlocal softtabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal colorcolumn=80
setlocal number
autocmd BufWritePre * %s/\s\+$//e

" Neomake is integrated with ShellCheck, this will enable it:
call neomake#configure#automake('nrwi', 500)
