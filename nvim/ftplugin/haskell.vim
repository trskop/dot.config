setlocal softtabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal colorcolumn=80
setlocal number

setlocal makeprg=stack\ test\ --no-run-tests\ --fast
setlocal errorformat=
  \%-G,
  \%-Z\ %#,
  \%W%f:%l:%c:\ Warning:\ %m,
  \%E%f:%l:%c:\ %m,
  \%E%>%f:%l:%c:,
  \%+C\ \ %#%m,
  \%W%>%f:%l:%c:,
  \%+C\ \ %#%tarning:\ %m,

augroup filetype_haskell
  autocmd! * <buffer>

  autocmd BufWritePre <buffer> %s/\s\+$//e

  " Long or badly placed multi-line strings cause syntax highlighting to fail.
  autocmd BufEnter <buffer> syntax sync fromstart
augroup END
