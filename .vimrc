set nocompatible

" Plugins {{{
call plug#begin('~/.vim/plugged')

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Initialize plugin system
call plug#end()

" }}}
" Defaults {{{
set showcmd
set hidden
set number
set modelines=0
set nomodeline
set ruler
set wildmode=list:full
set undolevels=100
set encoding=utf-8
set laststatus=2
set t_Co=256
set timeout
set timeoutlen=500
set ttimeoutlen=500
set synmaxcol=512
set ttyfast
set scrolloff=5
set backspace=indent,eol,start
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,*.pdf,*.bak,*.beam,*.pyc
set autoindent
set cindent

" Add subfolders recursively to path
set path+=**

" Add menu for tab completion
set wildmenu

" Fix certain issues with vim saving and file watchers. See:
" https://github.com/webpack/webpack/issues/781
set backupcopy:yes

" Set colorscheme
syntax on

set background=dark

" Use x system clipboard by default
if system('uname -s') == "Darwin\n"
  set clipboard=unnamed "OSX
else
  set clipboard=unnamedplus "Linux
endif

" Better pasting from system clipboard
set pastetoggle=<F2>

" Keep the backup, swap, and undo files separate from working dir.
silent !mkdir -p ~/.vim/.swp  ~/.vim/.backup ~/.vim/.undo
set swapfile
set directory=~/.vim/.swp
set backupdir=~/.vim/.backup
set undodir=~/.vim/.undo

" Set leader to space
let mapleader=" "

" Work having to use esc
inoremap jk <Esc>

" Split management.
set splitbelow
set splitright

" }}}
" Search Settings {{{
set hlsearch
set incsearch
nmap <Leader><Leader> :nohlsearch<CR>

" }}}
" Autogroups {{{
" Allow folding of vim comments
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END

" Automatically source Vimrc file on save.
augroup AutoCommands
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

" }}}
" Fugitive settings {{{
nmap <Leader>gs :Gstatus<cr>
nmap <Leader>gw :Gwrite<cr>
nmap <Leader>gc :Gcommit<cr>
nmap <Leader>gca :Gcommit --amend<cr>
nmap <Leader>gdd :Gdiff<cr>
nmap <Leader>gdc :Git diff --cached<cr>
nmap <Leader>gnb :Git checkout -b 
nmap <Leader>gb :Gblame<cr>
nmap <Leader>gl :Git! lola<cr>

" }}}
filetype plugin on
filetype indent on

