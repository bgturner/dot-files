
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

" end Base Settings }}}
" Plugins : Managed by vim-plug {{{
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
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
" General Mappings {{{
noremap <Leader><Leader> :nohlsearch<CR>

" }}}

