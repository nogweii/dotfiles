" vim: set foldmethod=marker"
" A simple single-file vim config file. A slightly comfy fallback from Neovim.
" Meant to emulate some of my common muscle-memorized key mappings and
" settings from neovim in environments where only Vim is available.

" {{{ Settings
set nocompatible               " Enable vim-only options
set nowrap                     " Don't wrap lines
set noerrorbells               " Disable any error bells
set nofoldenable               " Enable folding, at launch
set foldmethod=syntax          " Default to syntax based folds
set foldminlines=2             " Require at least 2 lines before closing a fold
set hlsearch                   " Highlight search results
set incsearch                  " Jump to the first match in real-time
set ignorecase                 " Case insensitive search, by default.
set smartcase                  " Case-sensitive if there any capital letters
set hidden                     " Allow changing buffers even with modifications
set spell                      " Enable spell check
set title                      " Modify the terminal title
set laststatus=2               " Always show the status bar
set ruler                      " Always show the position of the cursor
set showcmd                    " Show incomplete commands in the status bar
set autoindent                 " Automatically indent the next line
set cindent                    " Smart automatic indent
set linebreak                  " Don't wrap in the middle of a word
set modeline                   " Let individual files specify settings
set background=dark            " Use dark colors over lighter ones
set fileformat=unix            " Prefer UNIX line endings
set history=1000               " Remember lots of history for :
set wildmenu                   " Show completion matches in the status bar
set wildmode=longest,full      " Expand the longest common match, then all
set wildignore=*.o,*.obj,*~    " What to ignore in wildmenu
set backspace=indent,eol,start " Smart backspace in insert mode
set sidescroll=1               " Scroll horizontally 1 column at a time
set sidescrolloff=7            " Always show this at least this many columns
set enc=utf-8                  " Use UTF-8 encoding
set fenc=utf-8
set tenc=utf-8
set updatetime=2000            " Millisecs idle before calling the CursorHold
set shortmess=filnxtToOmIAr    " Use shorter messages in some ways
set softtabstop=2
set shiftwidth=2
set tabstop=4
set expandtab
set complete+=k,kspell         " Scan dictionaries for completion as well
set completeopt=menuone,longest,preview
set virtualedit+=block         " Block movement can go beyond end-of-line
set modelines=3                " Search the top and bottom 3 lines
execute 'set scrolloff='.(&lines-2)
set number

" Disable a lot of gui stuffs.
set guioptions-=TMRmaLr

" undo settings
set undofile

if exists("$XDG_DATA_HOME") && isdirectory($XDG_DATA_HOME . "/vim")
    " Store all of this data in a custom directory
    set viminfo+=n$XDG_DATA_HOME/vim/viminfo
    set undodir=$XDG_DATA_HOME/vim/
    set backupdir=$XDG_DATA_HOME/vim//
    set directory=$XDG_DATA_HOME/vim/
endif

" }}}

" {{{ Key bindings & mappings
" Unbind G, and make gG go to the bottom of the file
noremap  gG G
onoremap gG G
map      G <Nop>
" Set semicolon to map leader, and comma repeats the last f/t/F/T
noremap  , ;
map      ; <Nop>
let mapleader = ";"
" Use the recommended spelling
nmap     zp 1z=
nmap     <silent> zP :set spell!<CR>
map      <silent> <c-l> <c-l>:nohlsearch<CR>
vnoremap < <gv
vnoremap > >gv

" Swap the behaviors of ` & '
nnoremap ` '
nnoremap ' `
noremap  <expr> H (col('.') == matchend(getline('.'), '^\s*')+1 ? '0' : '^')
map      L $
nnoremap <silent> <C-n> :<C-U>SwitchToBuffer(v:count1)<CR>
nnoremap <silent> <C-p> :<C-U>SwitchToBuffer(-1*v:count1)<CR>
nmap     gQ gqap
imap     <C-j> <Down>
imap     <C-k> <Up>
imap     <C-h> <Left>
imap     <C-l> <Right>
nmap     <silent> ZW :update<CR>
nmap     ZD :call CleanClose(0)<CR>
nmap     ZE :e <C-R>=expand("%:h")<CR>/

inoremap <expr> <C-e> pumvisible() ? "\<c-e>" : "\<c-o>A"
imap     <expr> <C-a> "\<c-o>H"

nnoremap / /\v
vnoremap / /\v
" }}}

colorscheme slate
syntax on " Enable syntax highlighting
filetype on
filetype plugin on
filetype indent on
runtime macros/matchit.vim " Extend % matching
runtime ftplugin/man.vim " :Man command

" Switch to the next buffer (or previous, if passed a negative number)
function! SwitchToNextBuffer(incr)
    let current = bufnr("%")
    let last = bufnr("$")
    let new = current + a:incr
    while 1
        if new != 0 && buflisted(new)
            execute ":buffer ".new
            break
        else
            let new = new + a:incr
            if new < 1
                let new = last
            elseif new > last
                let new = 1
            endif
            if new == current
                break
            endif
        endif
    endwhile
endfunction

command! -nargs=1 SwitchToBuffer call SwitchToNextBuffer(<args>)

" Restore the cursor's position every time you open a file.
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

" Disable vim-as-man-pager within vim (so :Man works)
let $MANPAGER = ''

" Strip trailing whitespace just before saving a file
"autocmd BufWritePre * :%s/\s\+$//e

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" vim: set fdm=marker fileformat=unix:
