" {{{ Table of Contents -------------------------------------------------------
"
" ~/.config/nvim/init.vim
" |-- Basics
" |-- |-- Leader
" |   |-- Visuals
" |   `-- Python Provider
" |
" |-- Plugin
" |   |-- Dein.vim
" |   |-- Shougo/deoplete.vim
" |   |-- Shougo/neosnippet.vim
" |   |-- tomasr/molokai
" |   |-- neovimhaskell/haskell-vim
" |   |-- vim-airline/vim-airline
" |   |-- scrooloose/nerdtree
" |   |-- nathanaelkane/vim-indent-guides
" |   |-- parsonsmatt/intero-neovim
" |   |-- raichoo/purescript-vim
" |
" `-- Background Hack
"
" }}} Table of Contents -------------------------------------------------------

" {{{ Basics ------------------------------------------------------------------

" Minimal number of lines to be shown below and above current cursor position.
set scrolloff=2

" Show commands, as they are constructed, in the status line.
set showcmd

" {{{ Basics -- Leader --------------------------------------------------------

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
if ! exists("mapleader")
  let mapleader = ","
endif

if ! exists("g:mapleader")
  let g:mapleader = ","
endif

" }}} Basics -- Leader --------------------------------------------------------

" {{{ Basics -- Visuals -------------------------------------------------------

" Assume that terminal has dark background.
set background=dark

" GUI colours
highlight Normal guibg=black
highlight Normal guifg=white

" Use Unicode characters for separators.
" https://en.wikipedia.org/wiki/Box-drawing_character
set fillchars=vert:│,fold:─

" Enable the use of the mouse in a terminal.
set mouse=a

" }}} Basics -- Visuals -------------------------------------------------------

" {{{ Basics -- Python Provider -----------------------------------------------

let g:python_host_prog  = '/usr/bin/python2'
let g:python3_host_prog = '/usr/bin/python3'

" }}} Basics -- Python Provider -----------------------------------------------

" }}} Basics ------------------------------------------------------------------

" {{{ Plugin ------------------------------------------------------------------

" {{{ Plugin -- Dein.vim ------------------------------------------------------
"
" Dein.vm is a plugin manager.

" Required:
set runtimepath+=~/.config/nvim/dein.vim/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('~/.config/nvim/dein.vim')
  call dein#begin('~/.config/nvim/dein.vim')

  " Let dein manage dein
  " Required:
  call dein#add('~/.config/nvim/dein.vim/repos/github.com/Shougo/dein.vim')

  " {{{ Plugin -- Dein.vim -- Plugins to Install ------------------------------
  "
  " See ':help dein#add' and ':help dein-options' for more details.

  " Deoplete is the abbreviation of "dark powered neo-completion". It provides
  " an asynchronous keyword completion system in the current buffer.
  call dein#add('Shougo/deoplete.nvim')

  " The Neosnippet plug-In adds snippet support to Vim. Snippets are small
  " templates for commonly used code that you can fill in on the fly. To use
  " snippets can increase your productivity in Vim a lot. The functionality of
  " this plug-in is quite similar to plug-ins like snipMate.vim or
  " snippetsEmu.vim. But since you can choose snippets with the
  " neocomplcache/neocomplete interface, you might have less trouble using
  " them, because you do not have to remember each snippet name.
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')

  " Molokai is a Vim port of the monokai theme for TextMate originally created
  " by Wimer Hazenberg.
  call dein#add('tomasr/molokai')

  " All 256 xterm colors with their RGB equivalents, right in Vim!
  "
  " Provides command ':XtermColorTable'.
  call dein#add('guns/xterm-color-table.vim')

  " Lean & mean status/tabline for vim that's light as air.
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')

  " Indent Guides is a plugin for visually displaying indent levels in Vim.
  call dein#add('nathanaelkane/vim-indent-guides')

  " The NERD tree allows you to explore your filesystem and to open files and
  " directories. It presents the filesystem to you in the form of a tree which
  " you manipulate with the keyboard and/or mouse. It also allows you to
  " perform simple filesystem operations.
  call dein#add('scrooloose/nerdtree')

  " Asynchronous linting and make framework for Neovim/Vim.
  call dein#add('neomake/neomake')

  " A Git wrapper plugin that provides Git functionality from Neovim.
  call dein#add('tpope/vim-fugitive')

  " A Git commit browser / git log wrapper that extends fugitive.vim.
  call dein#add('int3/vim-extradite')

  " Syntax highlighting and indentation for Haskell and Cabal based on
  " https://github.com/idris-hackers/idris-vim
  call dein#add('neovimhaskell/haskell-vim')

  "call dein#add('parsonsmatt/intero-neovim')

  " Syntax highlighting and indentation for Purescript based on idris-vim and
  " haskell-vim.
  call dein#add('raichoo/purescript-vim')

  " Syntax highlighting for the Smart Game Format, which is the default
  " savegame format for the two-player Go game.
  call dein#add('vim-scripts/sgf.vim')

  " }}} Plugin -- Dein.vim -- Plugins to Install ------------------------------

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

" }}} Plugin -- Dein.vim ------------------------------------------------------

" {{{ Plugin -- Shougo/deoplete.vim -------------------------------------------

let g:deoplete#enable_at_startup = 1

" }}} Plugin -- Shougo/deoplete.vim -------------------------------------------

" {{{ Plugin -- Shougo/neosnippet.vim -----------------------------------------

" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
"imap <expr><TAB>
" \ pumvisible() ? "\<C-n>" :
" \ neosnippet#expandable_or_jumpable() ?
" \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif

" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1

" Additional snippets.
let g:neosnippet#snippets_directory='~/.config/snippets'

" }}} Plugin -- Shougo/neosnippet.vim -----------------------------------------

" {{{ Plugin -- tomasr/molokai ------------------------------------------------

colorscheme molokai

" }}} Plugin -- tomasr/molokai ------------------------------------------------

" {{{ Plugin -- neovimhaskell/haskell-vim -------------------------------------

let g:haskell_enable_quantification = 1   " highlight `forall`
let g:haskell_enable_recursivedo = 1      " highlight `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " highlight `proc`
let g:haskell_enable_pattern_synonyms = 1 " highlight `pattern`
let g:haskell_enable_typeroles = 1        " highlight type roles
let g:haskell_enable_static_pointers = 1  " highlight `static`
let g:haskell_backpack = 1                " highlight backpack keywords

" if bool
" >>>>then ...
" >>>>else ...
let g:haskell_indent_if = 4

" case xs of
" >>>>[]     -> ...
" >>>>(y:ys) -> ...
let g:haskell_indent_case = 4

" let x = 0 in
" >>>>x
let g:haskell_indent_let = 4

" where f :: Int -> Int
" >>>>>>f x = x
let g:haskell_indent_where = 6

" foo
" >>where
let g:haskell_indent_before_where = 2

" where
" >>foo
let g:haskell_indent_after_bare_where = 2

" do x <- a
" >>>y <- b
let g:haskell_indent_do = 3

" let x = 1
" in x
let g:haskell_indent_in = 0

" f x y
" >>|
let g:haskell_indent_guard = 2

" f xs ys = case xs of
" >>>>[]     -> ...
" >>>>(y:ys) -> ...
let g:haskell_indent_case_alternative = 4

" executable name
" >>main-is:             Main.hs
let g:cabal_indent_section = 2 " (limited to max. 4 spaces)

" }}} Plugin -- neovimhaskell/haskell-vim -------------------------------------

" {{{ Plugin -- vim-airline/vim-airline ---------------------------------------

let g:airline_powerline_fonts = 0

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" Following symbols don't work in rxvt-unicode:
let g:airline_symbols.maxlinenr = ' :'
let g:airline_symbols.crypt = 'cr'

let g:airline_theme='molokai'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_alt_sep = '│'

" }}} Plugin -- vim-airline/vim-airline ---------------------------------------

" {{{ Plugin -- scrooloose/nerdtree -------------------------------------------

" Close nerdtree after a file is selected
let NERDTreeQuitOnOpen = 1

function! IsNERDTreeOpen()
  return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

function! ToggleFindNerd()
  if IsNERDTreeOpen()
    exec ':NERDTreeToggle'
  else
    exec ':NERDTreeFind'
  endif
endfunction

" If nerd tree is closed, find current file, if open, close it.
nmap <silent> <leader>f <ESC>:call ToggleFindNerd()<CR>
nmap <silent> <leader>F <ESC>:NERDTreeToggle<CR>

" }}} Plugin -- scrooloose/nerdtree -------------------------------------------

" {{{ Plugin -- nathanaelkane/vim-indent-guides -------------------------------

let g:indent_guides_auto_colors = 1
let g:indent_guides_color_change_percent = 10
let g:indent_guides_guide_size = 1
let g:indent_guides_start_level = 2

" Terminal colours must be defined manually; g:indent_guides_auto_colors works
" only for GUI.
"
" Use e.g. :XtermColorTable to select colours.
highlight IndentGuidesOdd  ctermbg=234
highlight IndentGuidesEven ctermbg=236

" }}} Plugin -- nathanaelkane/vim-indent-guides -------------------------------

" {{{ Plugin -- parsonsmatt/intero-neovim -------------------------------------

" Process management:
nnoremap <Leader>hio :InteroOpen<CR>
nnoremap <Leader>hik :InteroKill<CR>
nnoremap <Leader>hic :InteroHide<CR>
nnoremap <Leader>hih :InteroHide<CR>
nnoremap <Leader>hil :InteroLoadCurrentModule<CR>

" REPL commands
nnoremap <Leader>hie :InteroEval<CR>
nnoremap <Leader>hit :InteroGenericType<CR>
nnoremap <Leader>hiT :InteroType<CR>
nnoremap <Leader>hii :InteroInfo<CR>
nnoremap <Leader>hiI :InteroTypeInsert<CR>

" Go to definition:
nnoremap <Leader>hid :InteroGoToDef<CR>

" Highlight uses of identifier:
nnoremap <Leader>hiu :InteroUses<CR>

" Reload the file in Intero after saving
"autocmd! BufWritePost *.hs InteroReload

" TODO: Implement this!
let g:intero_args = '-Wno-missing-import-lists'

" }}} Plugin -- parsonsmatt/intero-neovim -------------------------------------

" {{{ Plugin -- raichoo/purescript-vim ----------------------------------------

" if bool
" >>>>then ...
" >>>>else ...
let g:purescript_indent_if = 4

" case xs of
" >>>>[]     -> ...
" >>>>(y:ys) -> ...
let g:purescript_indent_case = 4

" let x = 0 in
" >>>>x
let g:purescript_indent_let = 4

" where f :: Int -> Int
" >>>>>>f x = x
let g:purescript_indent_where = 6

" do x <- a
" >>>y <- b
let g:purescript_indent_do = 3

" }}} Plugin -- raichoo/purescript-vim ----------------------------------------

" }}} Plugin ------------------------------------------------------------------
