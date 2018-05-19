set nocompatible

" Plugins {{{
call plug#begin('~/.vim/plugged')

" Misc
Plug 'junegunn/vim-easy-align'
Plug 'mattn/emmet-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Styling
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Color
Plug 'tomasr/molokai'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'morhetz/gruvbox'
Plug 'yuttie/hydrangea-vim'
Plug 'tyrannicaltoucan/vim-deep-space'
Plug 'AlessandroYorba/Despacio'
Plug 'cocopon/iceberg.vim'
Plug 'w0ng/vim-hybrid'

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
" Ctags {{{
" Rebuild ctags
nmap <leader>T :!ctags -R --exclude=node_modules --exclude=bower_components --exclude="*.min.js" --exclude="*jquery*.js" --exclude="*bootstrap*.js"<CR>

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

" Make emmet work with Sass
autocmd FileType html,css,scss,sass EmmetInstall

" Trim whitespace on save
autocmd FileType php,scss,sass,css,javascript,markdown autocmd BufWritePre <buffer> :%s/\s\+$//e

" }}}
" Netrw Settings {{{
let g:netrw_banner = 0

" Open Netrw in the project root
nnoremap <leader>ne :edit .<cr>

" Open Netrw in the path of the current buffer
nnoremap <leader>n. :edit %:p:h<cr>

" }}}
" FZF Settings {{{
" This is the default extra key bindings
" let g:fzf_action = {
"   \ 'ctrl-t': 'tab split',
"   \ 'ctrl-x': 'split',
"   \ 'ctrl-v': 'vsplit' }

" fzf layout
" - down / up / left / right
let g:fzf_layout = { 'down': '~50%' }

" Select multiple items with tab
nmap <Leader><tab> <plug>(fzf-maps-n)
xmap <Leader><tab> <plug>(fzf-maps-x)
omap <Leader><tab> <plug>(fzf-maps-o)

nnoremap <Leader>fe :Files<CR>
nnoremap <Leader>ft :Tags<CR>
nnoremap <Leader>fl :Lines<CR>
nnoremap <Leader>fb :Buffers<CR>
nnoremap <Leader>fh :Helptags<CR>
nnoremap <Leader>ff :Find 
nnoremap <Leader>f* :Find <C-R><C-W><CR>
nnoremap <silent> <Leader>fc :call fzf#run({
\   'source':
\     map(split(globpath(&rtp, "colors/*.vim"), "\n"),
\         "substitute(fnamemodify(v:val, ':t'), '\\..\\{-}$', '', '')"),
\   'sink':    'colo',
\   'options': '+m',
\   'left':    30
\ })<CR>

" Use Ripgrep if it's available
if executable('rg')
	" Replace the Files FZF command
	let $FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
	" Replace Grep
	command! -bang -nargs=* Find call fzf#vim#grep(
		\ 'rg --column --line-number --no-heading --follow --color=always --hidden '.<q-args>, 1,
		\ <bang>0 ? fzf#vim#with_preview('up:60%') : fzf#vim#with_preview('right:50%:hidden', '?'), <bang>0)
endif

" }}}
" Easy Align Settings {{{
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

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
" UltiSnips {{{
let g:UltiSnipsSnippetsDir         = $HOME.'/.vim/UltiSnips/'
let g:UltiSnipsSnippetDirectories  = ["UltiSnips"]
let g:UltiSnipsExpandTrigger       = "<c-j>"
let g:UltiSnipsJumpForwardTrigger  = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"
let g:UltiSnipsListSnippets        = "<c-h>"
let g:UltiSnipsEditSplit           = "vertical"

" }}}
" Quick file access {{{

" Add project subdirectories to path
set path+=**

" Easy editing of vimrc file.
nmap <Leader>ev :edit $MYVIMRC<CR>

" Open a new buffer using the current buffer's working directory
nmap <Leader>enb :edit %:p:h/

" end Quick file access }}}
filetype plugin on
filetype indent on

