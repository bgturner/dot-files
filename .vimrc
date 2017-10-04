set nocompatible

call plug#begin('~/.vim/plugged')

" Misc
Plug 'junegunn/vim-easy-align'
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
Plug 'Shougo/denite.nvim'
Plug 'tobyS/vmustache'
Plug 'vim-syntastic/syntastic'

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


" Initialize plugin system
call plug#end()

" defaults
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
colorscheme hybrid
let g:airline_theme='hybrid'

" Use x system clipboard by default
set clipboard=unnamedplus

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

" Split management.
set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Greplace Settings {{{

" Use 'ag' to search if it exists
if executable('ag')
  set grepprg=ag
  let g:grep_cmd_opts = '--line-numbers --noheading'
endif

nmap <Leader>grs :Gsearch<CR>
nmap <Leader>grr :Greplace<CR>

" }}}
" Netrw Settings {{{
let g:netrw_banner = 0

" Open Netrw in the project root
nnoremap <leader>ne :edit .<cr>

" Open Netrw in the path of the current buffer
nnoremap <leader>n. :edit %:p:h<cr>

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

" }}}

" Surround settings
let g:surround_{char2nr('b')} = "**\r**"
let g:surround_{char2nr('i')} = "_\r_"
let g:surround_{char2nr('s')} = "~~\r~~"

"
" Fugitive settings
"
nmap <Leader>gs :Gstatus<cr>
nmap <Leader>gw :Gwrite<cr>
nmap <Leader>gc :Gcommit<cr>
nmap <Leader>gca :Gcommit --amend<cr>
nmap <Leader>gdd :Gdiff<cr>
nmap <Leader>gdc :Git diff --cached<cr>
nmap <Leader>gnb :Git checkout -b 
nmap <Leader>gb :Gblame<cr>
nmap <Leader>gl :Git! lola<cr>


"
" PHP settings
"

" PHP refactoring settings
let g:vim_php_refactoring_auto_validate_visibility = 1
let g:vim_php_refactoring_default_property_visibility = 'public'
let g:vim_php_refactoring_default_method_visibility = 'public'

" php macros
"
" Creates class property and assigns argument from function signature within
" function block.
let @a = "mmyiw/}O	$this->\" = $\";O?__constructOprotected $\";`m:nohlsearch"

" Configure phpdocumentor
let g:pdv_template_dir = $HOME ."/.vim/dein/repos/github.com/tobyS/pdv/templates_snip"
nmap gk :call pdv#DocumentWithSnip()<CR>

" Setup phpcs rules for WP
let g:phpcs_std_list="WordPress-Docs, WordPress-VIP"
let g:phpcs_max_output = 2000 " Output limited to 2000 line
noremap <F5> <ESC>:Phpcs<CR>
noremap <F7> <ESC>:cprev<CR>
noremap <F8> <ESC>:cnext<CR>

"
" vim-php-namespace config.
"
" Automatically adds corresponding use statement for the class under the cursor.
function! IPhpInsertUse()
    call PhpInsertUse()
    call feedkeys('a',  'n')
endfunction
autocmd FileType php noremap <Leader>pnu :call PhpInsertUse()<CR>
"
" Expands the class name under the cursor to its fully qualified name.
function! IPhpExpandClass()
    call PhpExpandClass()
    call feedkeys('a', 'n')
endfunction
autocmd FileType php noremap <Leader>pne :call PhpExpandClass()<CR>
"
" end vim-php-namespace

" testing php
"
" [t]est [a]ll -- Run all tests defined for this project.
nmap <Leader>tea :!clear && phpunit<CR>
" [t]est [f]ile -- Only test the tests in the current buffer.
nmap <Leader>tef :!clear && phpunit %<CR>
" [t]est [t]est -- Run the test directly under the cursor.
nmap <Leader>tet ?function:set nohlsearch<cr>f wyiw:!clear && phpunit --filter "<cr>

"
" Quicker reference of php
"
" Open php docs for word under cursor.
" Note: Requires lynx editor.
"
" Opens web version of php docs.
function! OpenPHPManual(keyword)
  let web = 'lynx -accept_all_cookies --cookie_file=/home/benjamin/.lynx_cookies --cookie_save_file=/home/benjamin/.lynx_cookies --cookies'
  let url = 'http://jp2.php.net/' . a:keyword
  exec '!' . web . ' "' . url . '"'
endfunction

"
" For local reference download the php docs and save to the
" folder referenced below:
"
" http://php.net/download-docs.php
"
function! OpenPHPManualLocal(keyword)
  let web = 'lynx'
  let url = $HOME . '/.documentation/php-chunked-xhtml/function.' . a:keyword . '.html'
  exec '!' . web . ' "' . url . '"'
endfunction

noremap <Leader>dl :call OpenPHPManualLocal(expand('<cword>'))<CR>
noremap <Leader>dm :call OpenPHPManual(expand('<cword>'))<CR>

" Use pman for local documentation.
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

"
" End php documentation
"

"
" end PHP
"

"
" Syntastic
"
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

"
" end Syntastic
"

"UltiSnips
let g:UltiSnipsSnippetsDir         = $HOME.'/.vim/UltiSnips/'
let g:UltiSnipsSnippetDirectories  = ["UltiSnips"]
let g:UltiSnipsExpandTrigger       = "<c-j>"
let g:UltiSnipsJumpForwardTrigger  = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"
let g:UltiSnipsListSnippets        = "<c-h>"
let g:UltiSnipsEditSplit           = "vertical"


" Enable better matching of % with matchit plugin
runtime macros/matchit.vim

" Let Vim open Man pages
runtime ftplugin/man.vim



" Rebuild ctags
nmap <leader>T :!ctags -R --exclude=node_modules --exclude=bower_components --exclude="*.min.js" --exclude="*jquery*.js" --exclude="*bootstrap*.js"<CR>

" Make emmet work for sass files
autocmd FileType html,css,scss,sass EmmetInstall

" Transparent editing of gpg encrypted files.
" By Wouter Hanegraaff
augroup encrypted
  au!

  " First make sure nothing is written to ~/.viminfo while editing
  " an encrypted file.
  autocmd BufReadPre,FileReadPre *.gpg set viminfo=
  " We don't want a swap file, as it writes unencrypted data to disk
  autocmd BufReadPre,FileReadPre *.gpg set noswapfile

  " Switch to binary mode to read the encrypted file
  autocmd BufReadPre,FileReadPre *.gpg set bin
  autocmd BufReadPre,FileReadPre *.gpg let ch_save = &ch|set ch=2
  " (If you use tcsh, you may need to alter this line.)
  autocmd BufReadPost,FileReadPost *.gpg '[,']!gpg --decrypt 2> /dev/null

  " Switch to normal mode for editing
  autocmd BufReadPost,FileReadPost *.gpg set nobin
  autocmd BufReadPost,FileReadPost *.gpg let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost *.gpg execute ":doautocmd BufReadPost " . expand("%:r")

  " Convert all text to encrypted text before writing
  " (If you use tcsh, you may need to alter this line.)
  autocmd BufWritePre,FileWritePre *.gpg '[,']!gpg --default-recipient-self -ae 2>/dev/null
  " Undo the encryption so we are back in the normal text, directly
  " after the file has been written.
  autocmd BufWritePost,FileWritePost *.gpg u
augroup END

" EasyAlign settings.
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"
" Autocomplete
"
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3


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

" Make emmet work with Sass
autocmd FileType html,css,scss,sass EmmetInstall

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Trim whitespace on save
autocmd FileType php,scss,sass,css,javascript,markdown autocmd BufWritePre <buffer> :%s/\s\+$//e

" end Autocommand }}}

"
" Quick file access
"

" Easy editing of vimrc file.
nmap <Leader>ev :e $MYVIMRC<CR>

" Easy editing of snippets.
nmap <Leader>es :UltiSnipsEdit<CR>

"
" end Quick file access
"


" **************************
" Markdown
" **************************

" Disable instant markdown when opening markdown file.
" Note: the instant markdown plugin requires a node daemon:
"
" npm -g install instant-markdown-d
"
let g:instant_markdown_autostart = 0
nmap <Leader>mp :InstantMarkdownPreview<cr>

" interpret .md files as markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Spelling
nmap <Leader>ss :setlocal spell! spelllanguage=en_us<cr>

" Set wordy dictionaries
let g:wordy#ring = [
  \ 'weak',
  \ ['being', 'passive-voice', ],
  \ 'business-jargon',
  \ 'weasel',
  \ 'puffery',
  \ ['problematic', 'redundant', ],
  \ ['colloquial', 'idiomatic', 'similies', ],
  \ 'art-jargon',
  \ ['contractions', 'opinion', 'vague-time', 'said-synonyms', ],
  \ ]
" Easier wordy navigation
nnoremap <leader>sw K :NextWordy<cr>

" searching
set hlsearch
set incsearch
nmap <Leader><Leader> :nohlsearch<CR>

" Set better tabs
set tabstop=2 softtabstop=2 shiftwidth=2 noexpandtab
" set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab
" set tabstop=4 softtabstop=4 shiftwidth=4 expandtab
" set tabstop=2 softtabstop=2 shiftwidth=2 expandtab

filetype plugin on
filetype indent on

"
" Notes
"
" Default mappings for phpdocumentor
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
" nnoremap <unique> <Leader>da :call PhpDocAll()<CR>
"

