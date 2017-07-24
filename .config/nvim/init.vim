
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
Plug 'scrooloose/nerdtree'
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
Plug 'vim-syntastic/syntastic'
Plug 'neomake/neomake'
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'tobyS/vmustache'

" PHP
Plug 'tobyS/pdv'
Plug 'lvht/phpcd.vim', { 'for': 'php', 'do': 'composer install' }
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
" NERDTree Settings {{{
let g:NERDTreeWinSize = 40
nnoremap <Leader>nts :NERDTree 
nnoremap <Leader>ntt :NERDTreeToggle<cr>
nnoremap <Leader>ntf :NERDTreeFocus<cr>

" }}}
" Syntastic Settings {{{
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_loc_list_height = 5
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_error_symbol = '❌'
let g:syntastic_style_error_symbol = '⁉️'
let g:syntastic_warning_symbol = '⚠️'
let g:syntastic_style_warning_symbol = '💩'
highlight link SyntasticErrorSign SignColumn
highlight link SyntasticWarningSign SignColumn
highlight link SyntasticStyleErrorSign SignColumn
highlight link SyntasticStyleWarningSign SignColumn

let g:syntastic_mode_map = {
	\ "mode": "active",
	\ "active_filetypes": [],
	\ "passive_filetypes": ["php"] }

" Syntastic settings for phpcs and WordPress coding standards
"
" Run base PHP checker first, then run phpcs with WordPress standard
" If phpcs does not exist or the WordPress standard does not exist,
" Syntastic skips them (failing gracefully)
let g:syntastic_php_checkers = ['php','phpcs']
let g:syntastic_php_phpcs_args = '--standard=WordPress-VIP'

" If phpcs.xml is found, it supercedes the standard set above
let g:syntastic_php_phpcs_standard_file = "phpcs.xml"

nnoremap <Leader>sc :SyntasticCheck<cr>
nnoremap <Leader>sr :SyntasticReset<cr>

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
let g:neomake_php_phpcs_maker = {
	\ 'args': [
		\ '--report=csv',
		\ '--standard=WordPress-VIP',
	\ ],
	\ 'errorformat': 
		\ '%-GFile\,Line\,Column\,Type\,Message\,Source\,Severity%.%#,'.
		\ '"%f"\,%l\,%c\,%t%*[a-zA-Z]\,"%m"\,%*[a-zA-Z0-9_.-]\,%*[0-9]%.%#'
	\ }

" }}}
" NeoSnippet Settings {{{
let g:neosnippet#snippets_directory=$HOME.'/.vim/NeoSnippet/'

imap <C-j> <Plug>(neosnippet_expand_or_jump)
smap <C-j> <Plug>(neosnippet_expand_or_jump)
xmap <C-j> <Plug>(neosnippet_expand_target)

" }}}
" Deoplete Settings {{{

let g:deoplete#enable_at_startup = 1

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

