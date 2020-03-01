augroup filetype_markdown
  autocmd! * <buffer>
  autocmd FileType markdown setlocal spell
  autocmd BufNewFile,BufRead,BufEnter <buffer> setlocal spell

  " Long or badly placed code blocks cause syntax highlighting to fail.
  autocmd BufEnter <buffer> syntax sync fromstart
augroup END
