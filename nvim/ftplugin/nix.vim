setlocal number

" Long or badly placed multi-line strings cause syntax highlighting to fail.
autocmd BufEnter * syntax sync fromstart
