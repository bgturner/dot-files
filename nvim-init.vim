
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
" General Mappings {{{
noremap <Leader><Leader> :nohlsearch<CR>

" }}}

