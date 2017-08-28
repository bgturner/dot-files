
" Base Settings {{{
set number

" Tab menu
set wildmenu

" Set leader to space
let mapleader=' '

" Show what command is being created in the status line
set showcmd

" Allow buffers with unsaved changes to lose focus
set hidden

" Split management.
set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Use system clipboard -- requires xclip or xsel
set clipboard+=unnamedplus

" end Base Settings }}}
" Plugins : Managed by vim-plug {{{
call plug#begin('~/.vim/plugged')

" Misc
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf', { 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'Raimondi/delimitMate'
Plug 'terryma/vim-multiple-cursors'
Plug 'skwp/greplace.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'neomake/neomake'
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/denite.nvim'
Plug 'tobyS/vmustache'

" PHP
Plug 'tobyS/pdv'
Plug 'adoy/vim-php-refactoring-toolbox'
Plug 'StanAngeloff/php.vim'
Plug 'shawncplus/phpcomplete.vim'
Plug 'arnaud-lb/vim-php-namespace'
Plug 'joseluis/wordpress.vim'
Plug 'captbaritone/better-indent-support-for-php-with-html'

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Styling
Plug 'w0ng/vim-hybrid'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" end Plugins }}}
" Theme {{{

" Set colorscheme
syntax on
set background=dark
silent! colorscheme hybrid
let g:airline_theme='hybrid'

" end Theme }}}
" Autocompletion {{{
inoremap ^] ^X^]
inoremap ^F ^X^F
" end Autocompletion }}}
" Autocommand {{{

" Whitespace adjustments per filetype
autocmd Filetype markdown setlocal shiftwidth=2 tabstop=2

" Automatically source Vimrc file on save.
augroup AutoCommands
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

" Allow folding of vim comments
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END
" end Autocommand }}}
" Quick file access {{{

" Add project subdirectories to path
set path+=**

" Find a file in the path
nnoremap <Leader>ff :find 

" Easy editing of vimrc file.
nmap <Leader>ev :edit $MYVIMRC<CR>

" Open a new buffer using the current buffer's working directory
nmap <Leader>enb :edit %:p:h/

" Quick editing of pp notes
nmap <Leader>epp :edit ~/Documents/Notes/pp-notes.md<CR>

" Quick editing of pm notes
nmap <Leader>epm :edit ~/Documents/Notes/pm-notes.md<CR>

" end Quick file access }}}
" FZF Settings {{{

" FZF Mappings
" So many possibilties:
"
"     https://github.com/junegunn/fzf.vim#commands
"
nnoremap <Leader>fzf :FZF<cr>
nnoremap <Leader>fzg :GFiles<cr>
nnoremap <Leader>fza :Ag<cr>
nnoremap <Leader>fzt :Tags<cr>
nnoremap <Leader>fzb :Buffers<cr>

" }}}
" Netrw Settings {{{
let g:netrw_banner = 0
nnoremap <leader>fe :edit .<cr>

" }}}
" Greplace Settings {{{

" Use 'ag' to search if it exists
if executable('ag')
  set grepprg=ag
  let g:grep_cmd_opts = '--line-numbers --noheading'
endif

nmap <Leader>grs :Gsearch<CR>
nmap <Leader>grr :Greplace<CR>

" }}}
" Surround Settings {{{
let g:surround_{char2nr('b')} = "**\r**"
let g:surround_{char2nr('i')} = "_\r_"
let g:surround_{char2nr('s')} = "~~\r~~"

" }}}
" Neomake Settings {{{
nnoremap <Leader>nm :Neomake<cr>

let g:neomake_php_phpcs_maker = {
	\ 'args': [
		\ '--report=csv',
	\ ],
	\ 'errorformat': 
		\ '%-GFile\,Line\,Column\,Type\,Message\,Source\,Severity%.%#,'.
		\ '"%f"\,%l\,%c\,%t%*[a-zA-Z]\,"%m"\,%*[a-zA-Z0-9_.-]\,%*[0-9]%.%#'
	\ }

" }}}
" NeoSnippet Settings {{{
nnoremap <leader>es :NeoSnippetEdit -vertical<cr>
let g:neosnippet#snippets_directory=$HOME.'/.vim/NeoSnippet/'

imap <C-j> <Plug>(neosnippet_expand_or_jump)
smap <C-j> <Plug>(neosnippet_expand_or_jump)
xmap <C-j> <Plug>(neosnippet_expand_target)

" }}}
" Denite Settings {{{
nnoremap <Leader>ul :<C-u>Denite line<CR>
nnoremap <leader>ub :<C-u>Denite -buffer-name=buffers buffer<CR>
nnoremap <Leader>u? :<C-u>Denite -buffer-name=ag grep:.<CR>
nnoremap <leader>uf :<C-u>Denite -buffer-name=files file_rec<cr>
nnoremap <leader>ut :<C-u>Denite -buffer-name=tags tag<cr>
nnoremap <leader>uo :<C-u>Denite -buffer-name=outline outline<cr>
nnoremap <leader>urr :<C-u>Denite -resume<cr>
nnoremap <leader>ura :<C-u>Denite -buffer-name=ag -resume<cr>

" Change mappings.
call denite#custom#map(
      \ 'insert',
      \ '<C-j>',
      \ '<denite:move_to_next_line>',
      \ 'noremap'
      \)
call denite#custom#map(
      \ 'insert',
      \ '<C-k>',
      \ '<denite:move_to_previous_line>',
      \ 'noremap'
      \)

"
" Use ag for searching for files. Helpful for ignoring files that are ignored
" in .gitignore
"
call denite#custom#var('file_rec', 'command',
	\ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])

" Ag command on grep source
call denite#custom#var('grep', 'command', ['ag'])
call denite#custom#var('grep', 'default_opts',
		\ ['-i', '--vimgrep'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', [])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])


" Change ignore_globs
call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
      \ [ '.git/', '.ropeproject/', '__pycache__/',
      \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/',
      \   'vendor'])

nnoremap <Leader>umw :<C-U>Denite menu:workflow<CR>

" }}}
" Deoplete Settings {{{

let g:deoplete#enable_at_startup = 1

" Disable deoplete when in multi cursor mode
function! Multiple_cursors_before()
    let b:deoplete_disable_auto_complete = 1
endfunction
function! Multiple_cursors_after()
    let b:deoplete_disable_auto_complete = 0
endfunction

" }}}
" Fugitive Settings {{{

" Fugitive key mappings
nnoremap <Leader>gs :Gstatus<cr>
nnoremap <Leader>gw :Gwrite<cr>
nnoremap <Leader>gc :Gcommit<cr>
nnoremap <Leader>gca :Gcommit --amend<cr>
nnoremap <Leader>gb :Gblame<cr>
nnoremap <Leader>gdd :Gdiff<cr>
nnoremap <Leader>gdc :Git diff --cached<cr>
nnoremap <Leader>gbn :Git checkout -b 
nnoremap <Leader>gl :Git log --oneline --graph --decorate --all<cr>

" }}}
" Easy Align Settings {{{
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" }}}
" PHP Refactoring Settings {{{
" php refactoring default keymap:
"
" nnoremap <unique> <Leader>rlv :call PhpRenameLocalVariable()<CR>
" nnoremap <unique> <Leader>rcv :call PhpRenameClassVariable()<CR>
" nnoremap <unique> <Leader>rm :call PhpRenameMethod()<CR>
" nnoremap <unique> <Leader>eu :call PhpExtractUse()<CR>
" vnoremap <unique> <Leader>ec :call PhpExtractConst()<CR>
" nnoremap <unique> <Leader>ep :call PhpExtractClassProperty()<CR>
" vnoremap <unique> <Leader>em :call PhpExtractMethod()<CR>
" nnoremap <unique> <Leader>np :call PhpCreateProperty()<CR>
" nnoremap <unique> <Leader>du :call PhpDetectUnusedUseStatements()<CR>
" vnoremap <unique> <Leader>== :call PhpAlignAssigns()<CR>
" nnoremap <unique> <Leader>sg :call PhpCreateSettersAndGetters()<CR>
" nnoremap <unique> <Leader>cog :call PhpCreateGetters()<CR>
" nnoremap <unique> <Leader>da :call PhpDocAll()<CR>

" }}}
" PHP Namespace Plugin Settings {{{
"
" vim-php-namespace config.
"
" Automatically adds corresponding use statement for the class under the cursor.
function! IPhpInsertUse()
    call PhpInsertUse()
    call feedkeys('a', 'n')
endfunction
autocmd FileType php noremap <Leader>pnu :call PhpInsertUse()<CR>
"
" Expands the class name under the cursor to its fully qualified name.
function! IPhpExpandClass()
    call PhpExpandClass()
    call feedkeys('a', 'n')
endfunction
autocmd FileType php noremap <Leader>pne :call PhpExpandClass()<CR>

" }}}
" PHP Documentation {{{
"
" Use pman for local documentation.
"
" This makes use of Vim's keyword lookup program. Ensure that the php manual
" is installed locally. See: https://secure.php.net/download-docs.php
"
" Basically using PEAR you can install the documentation locally:
"
"		pear install doc.php.net/pman
"
" On the Vim side, we are replacing the keyword lookup for PHP files to use
" `pman`. For more info see `:help K`
autocmd FileType php set keywordprg=pman

" }}}
" PDV Settings {{{
let g:pdv_template_dir = $HOME ."/.vim/plugged/pdv/templates"
nnoremap <Leader>pd :call pdv#DocumentCurrentLine()<CR>

"}}}
" Phpcd Settings {{{

let g:deoplete#ignore_sources = get(g:, 'deoplete#ignore_sources', {})
let g:deoplete#ignore_sources.php = ['omni']

" }}}
" General Mappings {{{
noremap <Leader><Leader> :nohlsearch<CR>

" }}}

