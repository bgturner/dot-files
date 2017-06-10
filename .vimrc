set nocompatible

 " Install dein for package management if not installed.
 if !isdirectory($HOME.'/.vim/dein/repos/github.com/Shougo/dein.vim')
	 if executable('git')
		 call mkdir($HOME.'/.vim/dein/repos/github.com/Shougo/dein.vim', 'p')
		 !git clone https://github.com/Shougo/dein.vim $HOME/.vim/dein/repos/github.com/Shougo/dein.vim
	 endif
 endif

set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim " path to dein.vim

call dein#begin(expand('~/.vim/dein')) " plugins' root path

" The Shougo Way.
call dein#add('Shougo/dein.vim')
call dein#add('Shougo/vimproc.vim', {'build': 'make'})
call dein#add('Shougo/unite.vim')
call dein#add('Shougo/vimfiler.vim')
call dein#add('Shougo/neomru.vim')
call dein#add('Shougo/vimshell.vim')
call dein#add('Shougo/neocomplete.vim')
call dein#add('Shougo/unite-outline')
call dein#add('Shougo/context_filetype.vim')
call dein#add('Shougo/neoinclude.vim')
call dein#add('Shougo/neco-syntax')
call dein#add('Shougo/neopairs.vim')
call dein#add('tsukkee/unite-tag')
call dein#add('ujihisa/unite-colorscheme')

" Language specific things.
call dein#add('adoy/vim-php-refactoring-toolbox')
call dein#add('StanAngeloff/php.vim')
call dein#add('shawncplus/phpcomplete.vim')
call dein#add('phpcs.vim')
call dein#add('dsawardekar/wordpress.vim')
call dein#add('arnaud-lb/vim-php-namespace')
call dein#add('tobyS/vmustache')
call dein#add('tobyS/pdv')
call dein#add('jwalton512/vim-blade')


" misc
call dein#add('skwp/greplace.vim')
call dein#add('terryma/vim-multiple-cursors')
call dein#add('mattn/emmet-vim')
call dein#add('SirVer/ultisnips')
call dein#add('tpope/vim-repeat')
call dein#add('tpope/vim-surround')
call dein#add('tpope/vim-commentary')
call dein#add('junegunn/vim-easy-align')
call dein#add('airblade/vim-gitgutter')
call dein#add('Raimondi/delimitMate')
call dein#add('Konfekt/FastFold')
call dein#add('osyo-manga/unite-quickfix')

" Markdown and non-code writing
call dein#add('junegunn/goyo.vim')
call dein#add('junegunn/limelight.vim')
call dein#add('tpope/vim-markdown')
call dein#add('tpope/vim-fugitive')
call dein#add('reedes/vim-wordy')
call dein#add('reedes/vim-pencil')
call dein#add('reedes/vim-colors-pencil')
call dein#add('suan/vim-instant-markdown')
call dein#add('nelstrom/vim-markdown-folding')

" Theme things
call dein#add('vim-airline/vim-airline')
call dein#add('vim-airline/vim-airline-themes')
call dein#add('w0ng/vim-hybrid')

call dein#end()

" Automatically install plugins on startup.
if dein#check_install()
	call dein#install()
endif

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

" Fix certain issues with vim saving and file watchers. See:
" https://github.com/webpack/webpack/issues/781
set backupcopy:yes

" greplace.vim
set grepprg=ag
let g:grep_cmd_opts = '--line-numbers --noheading'
nmap <Leader>gs :Gsearch<CR>
nmap <Leader>gr :Greplace<CR>

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

" VimShell settings
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
let g:vimshell_right_prompt = 'strftime("%T")'
let g:vimshell_prompt = '$ '
nmap <leader>vs :VimShell<CR>

" Vimfiler settings
let g:loaded_netrwPlugin = 1 " Disable netrw.vim
let g:vimfiler_as_default_explorer = 1 " Replace netrw with vimfiler
nmap <leader>ff :<C-u>VimFiler<CR>
nmap <leader>fe :<C-u>VimFilerExplorer<CR>

" Unite settings
let g:unite_split_rule="botright"
let g:unite_source_file_mru_limit=300
let g:unite_source_history_yank_enable=1

nnoremap <Leader>us :<C-u>Unite -ignorecase -start-insert source<CR>
nnoremap <Leader>u/ :<C-u>Unite -ignorecase -start-insert line<CR>
nnoremap <leader>ub :<C-u>Unite -ignorecase -start-insert -buffer-name=buffers buffer<CR>
nnoremap <Leader>u? :<C-u>Unite -ignorecase -buffer-name=ag -no-split -silent grep:.<CR>
nnoremap <leader>uf :<C-u>Unite -ignorecase -buffer-name=files -no-split -force-redraw -start-insert file_rec/async<cr>
nnoremap <leader>ut :<C-u>Unite -ignorecase -buffer-name=tags -no-split -start-insert tag<cr>
nnoremap <leader>uo :<C-u>Unite -ignorecase -buffer-name=outline -silent -start-insert outline<cr>
nnoremap <leader>ur :<C-u>UniteResume<cr>
nnoremap <leader>un :<C-u>UniteNext<cr>
nnoremap <leader>up :<C-u>UnitePrevious<cr>


" Replace unite's grep with ag
" Note: must install ag -- sudo apt-get install silversearcher-ag
if executable('ag')
	let g:unite_source_grep_command = 'ag'
	let g:unite_source_grep_default_opts =
		\ '--nocolor --nogroup --smart-case --hidden' .
		\ ' --ignore .hg' .
		\ ' --ignore .svn' .
		\ ' --ignore .git' .
		\ ' --ignore .bzr' .
		\ ' --ignore node_modules' .
		\ ' --ignore bower_components'
	let g:unite_source_rec_async_command =
				\ ['ag', '--follow', '--nocolor', '--nogroup', '--hidden', '-g', '',
				\  '--ignore', 'bower_components',
				\  '--ignore', 'node_modules']
	" Replace vim's grep with ag
	set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
	set grepformat=%f:%l:%c:%m
endif " end ag customization

" Specific functionality only in the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
	" Play nice with supertab
	let b:SuperTabDisabled=1
	" Enable navigation with control-j and control-k in insert mode
	imap <buffer> <C-j>   <Plug>(unite_select_next_line)
	imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
endfunction

"
" Unite Custom Menus
"
" Initialize Unite's global list of menus.
if !exists('g:unite_source_menu_menus')
	let g:unite_source_menu_menus = {}
endif
" Enable quick access to all unite menus.
nnoremap <Leader>umm :<C-U>Unite menu -start-insert -ignorecase<CR>

"
" Workflow Menu
"
" Define menu.
let g:unite_source_menu_menus.workflow = {
	\ 'description': "Workflow"
	\ }
" Create a function to map 'Workflow' command labels to the commands that they execute.
function! g:unite_source_menu_menus.workflow.map(key, value)
	return {
		\ 'word': a:key,
		\ 'kind': 'command',
		\ 'action__command': a:value
		\ }
endfunction
" The labels to command map for the workflow menu.
" Note: the grep pattern with the colons follows -
" grep:<search_file_or_directory>:<grep_options>:<input_pattern>
let g:unite_source_menu_menus.workflow.command_candidates = [
	\ ['TODOs Project', 'Unite -start-insert -buffer-name=todos grep:.::TODO|todo'],
	\ ['TODOs Global', 'edit ~/Documents/todos/todos.md'],
	\ ['Notes Personal', 'edit ~/Documents/Notes/braindump.md'],
	\ ['Notes PeacefulMedia', 'edit ~/Projects/PeacefulMedia/notes/braindump.md']
	\ ]
nnoremap <Leader>umw :<C-U>Unite menu:workflow -start-insert -ignorecase<CR>

"
" Laravel Menu
"
" Define menu.
let g:unite_source_menu_menus.laravel = {
	\ 'description': "Laravel"
	\ }
" Create a function to map 'Laravel' command labels to the commands that they execute.
function! g:unite_source_menu_menus.laravel.map(key, value)
	return {
		\ 'word': a:key,
		\ 'kind': 'command',
		\ 'action__command': a:value
		\ }
endfunction
" The labels to map for the laravel menu.
let g:unite_source_menu_menus.laravel.command_candidates = [
	\ ['Routes', 'Unite -ignorecase -start-insert -buffer-name=laravelRoutes grep:.::^Route\:\:'],
	\ ['Migrations', 'Unite -ignorecase -start-insert -buffer-name=laravelMigrations grep:.::^class.*Migration'],
	\ ['Controllers', 'Unite -ignorecase -start-insert -buffer-name=laravelControllers grep:.::^class.*Controller'],
	\ ['Models', 'Unite -ignorecase -start-insert -buffer-name=laravelModels grep:.::^class.*Model']
	\ ]
nnoremap <Leader>uml :<C-U>Unite menu:laravel -start-insert -ignorecase<CR>

"
" end Unite Custom Menus
"


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
nmap <Leader>gdd :Gdiff<cr>
nmap <Leader>gdc :Git diff --cached<cr>
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



" Rebuild ctags
nmap <leader>T :!ctags -R --exclude=node_modules --exclude=bower_components<CR>

" Make emmet work for sass files
autocmd FileType html,css,scss,sass EmmetInstall

" EasyAlign settings.
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Auto Commands.


"
" Autocomand
"
" Automatically source Vimrc file on save.
augroup autosourcing
	autocmd!
	autocmd BufWritePost .vimrc source %
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

" Easily switch to writing mode
nmap <Leader>w :Goyo<bar>Pencil<CR>
nmap <Leader>W :Goyo!<bar>PencilOff<CR>

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

