", {{{ Table of Contents -------------------------------------------------------
"
" ~/.config/nvim/init.vim
" ├── Basics
" │   ├── Leader
" │   ├── Visuals
" │   └── Python Provider
" │
" ├── Plugin
" │   ├── Dein.vim
" │   ├── Shougo/deoplete.vim
" │   ├── Shougo/echodoc.vim
" │   ├── Shougo/neosnippet.vim
" │   ├── autozimu/LanguageClient-neovim
" │   ├── junegunn/fzf.vim
" │   ├── tomasr/molokai
" │   ├── neovimhaskell/haskell-vim
" │   ├── vim-airline/vim-airline
" │   ├── scrooloose/nerdtree
" │   ├── scrooloose/nerdcommenter
" │   ├── nathanaelkane/vim-indent-guides
" │   └── raichoo/purescript-vim
" │
" ├── Terminal
" │
" ├── Syntax Highlighting Settings
" │
" └── Local Configuration
"
" }}} Table of Contents -------------------------------------------------------

" {{{ Basics ------------------------------------------------------------------

" Minimal number of lines to be shown below and above current cursor position.
set scrolloff=2

" Show commands, as they are constructed, in the status line.
set showcmd

" Show the effects of a command incrementatlly, as you type.
set inccommand=nosplit

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

if has('macunix')
  let g:python_host_prog  = '/usr/local/bin/python2'
  let g:python3_host_prog = '/usr/local/bin/python3'
else
  let g:python_host_prog  = '/usr/bin/python2'
  let g:python3_host_prog = '/usr/bin/python3'
endif

" }}} Basics -- Python Provider -----------------------------------------------

" }}} Basics ------------------------------------------------------------------

" {{{ Plugin ------------------------------------------------------------------

" {{{ Plugin -- Dein.vim ------------------------------------------------------
"
" Dein.vim is a plugin manager.

" Required:
set runtimepath+=~/.cache/dein.vim/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('~/.cache/dein.vim')
  call dein#begin('~/.cache/dein.vim')

  " Let dein manage dein
  " Required:
  call dein#add('~/.cache/dein.vim/repos/github.com/Shougo/dein.vim')

  " {{{ Plugin -- Dein.vim -- Plugins to Install ------------------------------
  "
  " See ':help dein#add' and ':help dein-options' for more details.
  "
  " Note:
  "
  " * Always add links to GitHub repos so that it's easy to open it in a
  "   browser via `gx`.

  " Deoplete is the abbreviation of "dark powered neo-completion". It provides
  " an asynchronous keyword completion system in the current buffer.
  "
  " <https://github.com/Shougo/deoplete.nvim>
  call dein#add('Shougo/deoplete.nvim')

  " The Neosnippet plug-In adds snippet support to Vim. Snippets are small
  " templates for commonly used code that you can fill in on the fly. To use
  " snippets can increase your productivity in Vim a lot. The functionality of
  " this plug-in is quite similar to plug-ins like snipMate.vim or
  " snippetsEmu.vim. But since you can choose snippets with the
  " neocomplcache/neocomplete interface, you might have less trouble using
  " them, because you do not have to remember each snippet name.
  "
  " <https://github.com/Shougo/neosnippet.vim>
  " <https://github.com/Shougo/neosnippet-snippets>
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')

  " Displays function signatures from completions in the command line.
  "
  " <https://github.com/Shougo/echodoc.vim>
  call dein#add('Shougo/echodoc.vim')

  " Denite is a dark powered plugin for Neovim/Vim to unite all interfaces. It
  " can replace many features or plugins with its interface. It is like a
  " fuzzy finder, but is more generic. You can extend the interface and create
  " the sources.
  "
  " <https://github.com/Shougo/denite.vim>
  call dein#add('Shougo/denite.nvim')

  " Fzf is a general-purpose command-line fuzzy finder.
  "
  " It's an interactive Unix filter for command-line that can be used with any
  " list; files, command history, processes, hostnames, bookmarks, git
  " commits, etc.
  "
  " Repository 'junegunn/fzf' is used to install Fzf command-line tool. It also
  " provides basic Vim/Neovim integration (:FZF command). Full Vim/Neovim
  " integration is provided by 'junegunn/fzf.vim'.
  "
  " Install command `./install --bin` is there so the interactive script
  " doesn't block.
  "
  " Instructions taken from <https://github.com/Shougo/dein.vim/issues/74>
  "
  " <https://github.com/junegunn/fzf>
  " <https://github.com/junegunn/fzf.vim>
  call dein#add('junegunn/fzf', { 'build': './install --bin', 'merged': 0 })
  call dein#add('junegunn/fzf.vim', { 'depends': 'fzf' })

  " Molokai is a Vim port of the monokai theme for TextMate originally created
  " by Wimer Hazenberg.
  "
  " <https://github.com/tomasr/molokai>
  call dein#add('tomasr/molokai')

  " All 256 xterm colors with their RGB equivalents, right in Vim!
  "
  " Provides commands such as: ':XtermColorTable'.
  "
  " Help:
  "   :help xterm-color-table.txt
  "
  " <https://github.com/guns/xterm-color-table.vim>
  call dein#add('guns/xterm-color-table.vim')

  " Lean & mean status/tabline for vim that's light as air.
  "
  " <https://github.com/vim-airline/vim-airline>
  " <https://github.com/vim-airline/vim-airline-themes>
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')

  " Indent Guides is a plugin for visually displaying indent levels in Vim.
  "
  " <https://github.com/nathanaelkane/vim-indent-guides>
  call dein#add('nathanaelkane/vim-indent-guides')

  " The NERD tree allows you to explore your filesystem and to open files and
  " directories. It presents the filesystem to you in the form of a tree which
  " you manipulate with the keyboard and/or mouse. It also allows you to
  " perform simple filesystem operations.
  "
  " Plugin 'Xuyuanp/nerdtree-git-plugin' provides functionality for showing
  " Git flags (modified, staged, untracked, etc.) in NERDTree.
  "
  " <https://github.com/scrooloose/nerdtree>
  " <https://github.com/Xuyuanp/nerdtree-git-plugin>
  call dein#add('scrooloose/nerdtree')
  call dein#add('Xuyuanp/nerdtree-git-plugin')

  " Functions for manipulation of comments in source code. For example,
  " commenting-out lines.
  "
  " <https://github.com/scrooloose/nerdcommenter>
  call dein#add('scrooloose/nerdcommenter')

  " Asynchronous linting and make framework for Neovim/Vim.
  "
  " <https://github.com/neomake/neomake>
  call dein#add('neomake/neomake')

  " A Git wrapper plugin that provides Git functionality from Neovim.
  "
  " <https://github.com/tpope/vim-fugitive>
  call dein#add('tpope/vim-fugitive')

  " A Git commit browser / git log wrapper that extends fugitive.vim.
  "
  " <https://github.com/int3/vim-extradite>
  call dein#add('int3/vim-extradite')

  " Syntax highlighting and indentation for Haskell and Cabal based on
  " <https://github.com/idris-hackers/idris-vim>
  "
  " <https://github.com/neovimhaskell/haskell-vim>
  call dein#add('neovimhaskell/haskell-vim')

  " Language Server Protocol support for Neovim. See
  " <https://github.com/Microsoft/language-server-protocol> for more
  " information.
  "
  " Installation instructions:
  "   <https://github.com/autozimu/LanguageClient-neovim/blob/next/INSTALL.md#deinvim-user>
  "
  " <https://github.com/autozimu/LanguageClient-neovim>
  call dein#add('autozimu/LanguageClient-neovim', {
    \ 'rev': 'next',
    \ 'build': 'bash install.sh',
    \ })

  " Syntax highlighting and indentation for Purescript based on idris-vim and
  " haskell-vim.
  "
  " <https://github.com/raichoo/purescript-vim>
  call dein#add('raichoo/purescript-vim')

  " Syntax highlighting for the Smart Game Format, which is the default
  " savegame format for the two-player Go game.
  "
  " <https://github.com/vim-scripts/sgf.vim>
  call dein#add('vim-scripts/sgf.vim')

  " Syntax highlighting for PlantUML <http://plantuml.com>.
  "
  " <https://github.com/aklt/plantuml-syntax>
  call dein#add('aklt/plantuml-syntax')

  " Syntax highlighting for Dhall
  "
  " <https://github.com/dhall-lang/dhall-lang>
  call dein#add('vmchale/dhall-vim')

  " Extra icons for various Vim/Neovim plugins. It supports plugins such as
  " NERDTree, vim-airline, CtrlP, powerline, denite, unite, lightline.vim,
  " vim-startify, vimfiler, vim-workspace and flagship.
  "
  " See also: <https://github.com/ryanoasis/nerd-fonts>
  "
  " <https://github.com/ryanoasis/vim-devicons>
  call dein#add('ryanoasis/vim-devicons')

  " Syntax highlighting for Nix expressions and other functionality that makes
  " Nix files editing better.
  "
  " Interesting article: <http://nicknovitski.com/vim-nix-syntax>
  "
  " <https://github.com/LnL7/vim-nix>
  call dein#add('LnL7/vim-nix')

  " Direnv integration that correctly loads Direnv environment, and provides
  " syntax highlighting for `.envrc` files.
  "
  " <https://github.com/direnv/direnv.vim>
  call dein#add('direnv/direnv.vim')

  " LookML syntax highlighting.
  "
  " <https://github.com/thalesmello/lkml.vim>
  call dein#add('thalesmello/lkml.vim')

  " Call Ripgrep (`rg`) from Neovim.
  "
  " Usage:
  "   :Rg <string|pattern>
  "
  " Configuration:
  "   <https://github.com/jremmen/vim-ripgrep#configuration>
  "
  " <https://github.com/jremmen/vim-ripgrep>
  if executable('rg')
    call dein#add('jremmen/vim-ripgrep')
  endif

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

" {{{ Plugin -- Shougo/echodoc.vim --------------------------------------------

" Prevent mode information in cmdline to overwrite echodoc's output:
set cmdheight=2

" }}} Plugin -- Shougo/echodoc.vim --------------------------------------------

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

" {{{ Plugin -- autozimu/LanguageClient-neovim --------------------------------

" Required for operations modifying multiple buffers like rename.
set hidden

" Automatically start language servers.
let g:LanguageClient_autoStart = 1

let g:LanguageClient_selectionUI = "fzf"
"let g:LanguageClient_selectionUI = "location-list"

let g:LanguageClient_serverCommands = {}

if executable('ghcide')
  let g:LanguageClient_serverCommands['haskell'] = ['ghcide', '--lsp']
elseif executable('hie-wrapper')
  let g:LanguageClient_serverCommands['haskell'] = ['hie-wrapper', '--lsp']
elseif executable('hie')
  let g:LanguageClient_serverCommands['haskell'] = ['hie', '--lsp']
endif

augroup LanguageClient_signcolumn
  autocmd!
  autocmd User LanguageClientStarted setlocal signcolumn=yes
  autocmd User LanguageClientStopped setlocal signcolumn=auto
augroup END

nmap <silent> <leader>c <ESC>:call LanguageClient_contextMenu()<CR>
nmap <silent> <leader>d <ESC>:call LanguageClient_textDocument_definition()<CR>
nmap <silent> <leader>D <ESC>:call LanguageClient_textDocument_hover()<CR>

" }}} Plugin -- autozimu/LanguageClient-neovim --------------------------------

" {{{ Plugin -- junegunn/fzf.vim ----------------------------------------------

if has('nvim') || has('gui_running')
  let $FZF_DEFAULT_OPTS .= ' --inline-info'
endif

" Hide statusline of terminal buffer
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

let g:fzf_colors = {
  \ 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment']
  \ }

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

" nnoremap <silent> <Leader><Leader> :Files<CR>
nnoremap <silent> <expr> <Leader><Leader> (expand('%') =~ 'NERD_tree' ? "\<c-w>\<c-w>" : '').":Files\<cr>"

nnoremap <silent> <Leader><Enter>  :Buffers<CR>
nnoremap <silent> <Leader>l        :Lines<CR>
nnoremap <silent> <Leader>`        :Marks<CR>

nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

"inoremap <expr> <c-x><c-t> fzf#complete('tmuxwords.rb --all-but-current --scroll 500 --min 5')
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
inoremap <expr> <c-x><c-d> fzf#vim#complete#path('blsd')
imap <c-x><c-l> <plug>(fzf-complete-line)

" }}} Plugin -- junegunn/fzf.vim ----------------------------------------------

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

let g:airline_theme='molokai'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

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

" {{{ Plugin -- scrooloose/nerdcommenter --------------------------------------

" Insert spaces after comment delimiters.
let g:NERDSpaceDelims = 1

" Allow commenting and inverting empty lines (useful when commenting a
" region).
let g:NERDCommentEmptyLines = 1

" Enable trimming of trailing whitespace when uncommenting.
let g:NERDTrimTrailingWhitespace = 1

" Use alternative delimiter "--" by default on Haskell files.
let g:NERDAltDelims_haskell = 1

" }}} Plugin -- scrooloose/nerdcommenter --------------------------------------

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

nmap <silent> <Leader>ig <Plug>IndentGuidesToggle

" }}} Plugin -- nathanaelkane/vim-indent-guides -------------------------------

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

" {{{ Terminal ----------------------------------------------------------------

nmap <silent> <leader>t <ESC>:belowright split +terminal<CR>
nmap <silent> <leader>T <ESC>:belowright vsplit +terminal<CR>

" }}} Terminal ----------------------------------------------------------------

" {{{ Syntax Highlighting Settings --------------------------------------------

" This is to help syntax highlighting figure out which croup it is in.  For
" example multiline strings can break syntax highlighting.  This may not be
" the best default for all syntaxes, in which case it would be better to put
" specifics into appropriate ftplugins.
"
" Other options that may be interesting to look at are:
"
" * `:set redrawtime`
" * `:syntax sync fromstart`
"
" Some relevant sources:
"
" * `:help syntax`
" * `:help 'redrawtime'`
" * <https://vim.fandom.com/wiki/Fix_syntax_highlighting>
syntax sync minlines=500

" }}} Syntax Highlighting Settings --------------------------------------------

" {{{ Local Configuration -----------------------------------------------------

function! SourceLocalConfig(file)
  if filereadable(expand(a:file))
    execute 'source' a:file
  endif
endfunction

call SourceLocalConfig('~/.local/src/localhost/dot.config/nvim/init-local.vim')

" }}} Local Configuration -----------------------------------------------------
