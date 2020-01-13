autocmd BufNewFile,BufRead,BufEnter * setlocal spell

" Long or badly placed code blocks cause syntax highlighting to fail.
autocmd BufEnter * syntax sync fromstart
