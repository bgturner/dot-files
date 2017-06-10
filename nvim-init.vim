
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

Plug 'Shougo/vimfiler.vim'
Plug 'w0ng/vim-hybrid'

call plug#end()

" end Plugins }}}
" Theme {{{

" Set colorscheme
syntax on
set background=dark
silent! colorscheme hybrid

" end Theme }}}
" Autocommand {{{

" Automatically source Vimrc file on save.
augroup AutoCommands
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

" end Autocommand }}}
" Vimfiler Settings {{{

" Vimfiler keyboard shortcuts - mnemonic *f*ile *e*ditor
nnoremap <Leader>fef :VimFiler<cr>
nnoremap <Leader>fed :VimFilerDouble<cr>
nnoremap <Leader>fee :VimFilerExplorer<cr>
nnoremap <Leader>fes :VimFilerSimple<cr>

" Set Vimfiler as default file explorer
let g:loaded_netrwPlugin = 1 " Disable netrw.vim
let g:vimfiler_as_default_explorer = 1 " Vimfiler as default

" end Vimfiler Settings }}}

