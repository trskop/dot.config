autocmd BufNewFile,BufRead,BufEnter * set spell

" Long or badly placed code blocks cause syntax highlighting to fail.
autocmd BufEnter * syntax sync fromstart
