
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

" PHP
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

call plug#end()

" end Plugins }}}
" Theme {{{

" Set colorscheme
syntax on
set background=dark
silent! colorscheme hybrid

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
" Surround Settings {{{
let g:surround_{char2nr('b')} = "**\r**"
let g:surround_{char2nr('i')} = "_\r_"
let g:surround_{char2nr('s')} = "~~\r~~"

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
" General Mappings {{{
noremap <Leader><Leader> :nohlsearch<CR>

" }}}

