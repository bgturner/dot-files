set nocompatible

" Plugins {{{
call plug#begin('~/.vim/plugged')

" Misc
Plug 'junegunn/vim-easy-align'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'editorconfig/editorconfig-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'adelarsq/vim-matchit'
Plug 'sheerun/vim-polyglot'
Plug 'majutsushi/tagbar'

" Styling
Plug 'itchyny/lightline.vim'
Plug 'morhetz/gruvbox'

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
set backspace=indent,eol,start
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,*.pdf,*.bak,*.beam,*.pyc
set autoindent
set cindent
set nowrap

" Add subfolders recursively to path
set path+=**

" Add menu for tab completion
set wildmenu

" Fix certain issues with vim saving and file watchers. See:
" https://github.com/webpack/webpack/issues/781
set backupcopy:yes

" Set colorscheme
syntax on
colorscheme gruvbox
set background=dark

" Use x system clipboard by default
if system('uname -s') == "Darwin\n"
  set clipboard=unnamed "OSX
else
  set clipboard=unnamedplus "Linux
endif

" Better pasting from system clipboard
set pastetoggle=<F2>

" Quick saving of file
nnoremap <leader>s :w<cr>

" Toggle display of whitespace
nnoremap <leader>l :set list!<cr>

" Tabstops
nnoremap <leader>t2 :set expandtab shiftwidth=2 tabstop=2<cr>
nnoremap <leader>t4 :set expandtab shiftwidth=4 tabstop=4<cr>
" nnoremap <leader>t2 :set noexpandtab shiftwidth=2 tabstop=2<cr>

" scroll the viewport faster
nnoremap <C-e> 4<C-e>
nnoremap <C-y> 4<C-y>

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
" Lightline {{{
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \ },
      \ }

function! LightlineFilename()
  let filename = expand('%:F') !=# '' ? expand('%:F') : '[No Name]'
  let modified = &modified ? ' +' : ''
  return filename . modified
endfunction
set laststatus=2                                                "vim-lightline
set noshowmode                                                  "vim-lightline

" }}}
" General Remaps {{{

" Open new line below and above current line
nnoremap <leader>o o<esc>
nnoremap <leader>O O<esc>

" ----------------------------------------------------------------------------
" Quickfix
" ----------------------------------------------------------------------------
nnoremap ]q :cnext<cr>zz
nnoremap [q :cprev<cr>zz
nnoremap ]l :lnext<cr>zz
nnoremap [l :lprev<cr>zz

" ----------------------------------------------------------------------------
" Buffers
" ----------------------------------------------------------------------------
nnoremap ]b :bnext<cr>
nnoremap [b :bprev<cr>
nnoremap <Leader>bnh :split edit<cr>
nnoremap <Leader>bnv :vertical split edit<cr>
nnoremap <Leader>bc :close<cr>
nnoremap <Leader>bd :bdelete<cr>

" ----------------------------------------------------------------------------
" Tabs
" ----------------------------------------------------------------------------
nnoremap ]t :tabn<cr>
nnoremap [t :tabp<cr>
nnoremap <Leader>tn :tabnew<cr>
nnoremap <Leader>tc :tabclose<cr>

" ----------------------------------------------------------------------------
" Moving lines
" ----------------------------------------------------------------------------
nnoremap <silent> <C-k> :move-2<cr>
nnoremap <silent> <C-j> :move+<cr>
nnoremap <silent> <C-h> <<
nnoremap <silent> <C-l> >>
xnoremap <silent> <C-k> :move-2<cr>gv
xnoremap <silent> <C-j> :move'>+<cr>gv
xnoremap <silent> <C-h> <gv
xnoremap <silent> <C-l> >gv
xnoremap < <gv
xnoremap > >gv

" ----------------------------------------------------------------------------
" <Leader>c Close quickfix/location window
" ----------------------------------------------------------------------------
nnoremap <leader>q :call QuickfixToggle()<cr>
nnoremap <leader>l :call LocationListToggle()<cr>

let g:quickfix_is_open = 0
function! QuickfixToggle()
    if g:quickfix_is_open
        cclose
        let g:quickfix_is_open = 0
        execute g:quickfix_return_to_window . "wincmd w"
    else
        let g:quickfix_return_to_window = winnr()
        copen
        let g:quickfix_is_open = 1
    endif
endfunction

let g:location_list_is_open = 0
function! LocationListToggle()
    if g:location_list_is_open
        lclose
        let g:location_list_is_open = 0
        execute g:location_list_return_to_window . "wincmd w"
    else
        let g:location_list_return_to_window = winnr()
        lopen
        let g:location_list_is_open = 1
    endif
endfunction

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
let g:netrw_liststyle = 3

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

nnoremap <Leader>pf :Files<CR>
nnoremap <Leader>ft :Tags<CR>
nnoremap <Leader>ji :Lines<CR>
nnoremap <Leader>bb :Buffers<CR>
nnoremap <Leader>hh :Helptags<CR>
nnoremap <Leader>fs :Snippets<CR>
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

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

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
" Quick file access {{{

" Add project subdirectories to path
set path+=**

" Easy editing of vimrc file.
nmap <Leader>ev :edit $MYVIMRC<CR>

" Open a new buffer using the current buffer's working directory
nmap <Leader>enb :edit %:p:h/

" end Quick file access }}}

" " omnifuncs
augroup omnifuncs
  autocmd!
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup end

let g:pdv_template_dir = $HOME ."/.vim/bundle/pdv/templates_snip"
nnoremap <buffer> <C-p> :call pdv#DocumentWithSnip()<CR>

filetype plugin on
filetype indent on

