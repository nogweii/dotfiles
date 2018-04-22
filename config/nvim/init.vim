" NeoVim config file, created by Evaryont. <hello@evaryont.me>
"
" A lot of this is stuff I've run into over the years, from various forums
" and my own experimentation. Some of it is archaic and silly, others neat
" or interesting. I hope you agree, and that you may find something that
" intrigues you as well.
" -- Happy vimming! :smile

" Get the directory that contains init.vim.
let $VIMUSERRUNTIME = fnamemodify($MYVIMRC, ':p:h')

" {{{ Automatically install vim-plug if it isn't already present
let s:user_plug_vimscript = $VIMUSERRUNTIME . '/autoload/plug.vim'
if empty(glob(s:user_plug_vimscript))
  silent execute '!curl -fLo ' . s:user_plug_vimscript . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
let s:user_plug_helpdoc = $VIMUSERRUNTIME . '/doc/plug.txt'
if empty(glob(s:user_plug_helpdoc))
  silent execute '!curl -fLo ' . s:user_plug_helpdoc . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/doc/plug.txt'
  autocmd VimEnter * helptags $VIMUSERRUNTIME/doc
endif
" }}}

" {{{ Use vim-plug to install a bunch of plugins:
call plug#begin($VIMUSERRUNTIME . '/plugged')
Plug 'junegunn/vim-easy-align'
Plug 'sheerun/vim-polyglot', { 'tag': '*' }
Plug 'editorconfig/editorconfig-vim'
Plug 'w0rp/ale', { 'tag': '*' }

"Plug 'junegunn/vim-peekaboo'
Plug 'justinmk/vim-dirvish'
Plug 'justinmk/vim-gtfo'

" Tim Pope series of plugins. Quite a prolific vimscript author!
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-fugitive'

Plug 'raimondi/delimitmate'
Plug 'raimondi/yaifa'

Plug 'machakann/vim-highlightedyank'
Plug 'kien/rainbow_parentheses.vim'
Plug 'andrewradev/sideways.vim'
Plug 'mjbrownie/swapit'
Plug 'chr4/sslsecure.vim'
Plug 'tweekmonster/startuptime.vim'

" Various color schemes I have tried...
Plug 'morhetz/gruvbox'
Plug 'nanotech/jellybeans.vim'
Plug 'dracula/vim'
Plug 'jnurmine/zenburn'
Plug 'tpope/vim-vividchalk'
Plug 'jonathanfilip/vim-lucius'
Plug 'junegunn/seoul256.vim'
Plug 'tomasr/molokai'
Plug 'chriskempson/base16-vim'
Plug 'nlknguyen/papercolor-theme'
Plug 'w0ng/vim-hybrid'
Plug 'kristijanhusak/vim-hybrid-material'

" " When I change my init.vim, auto-update everything
" if getftime($MYVIMRC) > getftime($VIMUSERRUNTIME . '/plugged')
"   augroup plug_autoupdate
"     autocmd!
"     autocmd VimEnter * PlugUpdate | PlugUpgrade | PlugClean!
"   augroup END
" endif

call plug#end() " }}}

" {{{ Autocommand groups
augroup rainbow_parentheses
  autocmd!
  au VimEnter * RainbowParenthesesToggle
  au Syntax * RainbowParenthesesLoadRound
  au Syntax * RainbowParenthesesLoadSquare
  au Syntax * RainbowParenthesesLoadBraces
augroup END

augroup vimrc_folding
  autocmd!
  au BufReadPost $MYVIMRC setlocal foldmethod=marker
augroup END

augroup recalculate_scrolloffset
  autocmd!
  au VimResized * execute 'set scrolloff='.(&lines-2)
augroup END
" }}}

" {{{ NeoVim settings
set nowrap                     " Don't wrap lines
set noerrorbells               " Disable any error bells
set laststatus=2               " Always show the status bar
set hidden                     " Allow changing buffers even with modifications
set spell                      " Enable spell check
set title                      " Modify the terminal title
set foldmethod=syntax          " Default to syntax based folds
set foldminlines=2             " Require at least 2 lines before closing a fold
set hlsearch                   " Highlight search results
set incsearch                  " Jump to the first match in real-time
set ignorecase                 " Case insensitive search, by default.
set smartcase                  " Case-sensitive if there any capital letters
set modeline                   " Let individual files specify settings
set background=dark            " Use dark colors over lighter ones
set fileformat=unix            " Prefer UNIX line endings
set history=10000              " Remember lots of history for :
set wildmenu                   " Show completion matches in the status bar
set wildmode=longest,full      " Expand the longest common match, then all
set wildignore=*.o,*.obj,*~    " What to ignore in wildmenu
set backspace=indent,eol,start " Smart backspace in insert mode
set sidescroll=1               " Scroll horizontally 1 column at a time
set sidescrolloff=7            " Always show this at least this many columns
set fileencoding=utf-8         " Default to assuming files are encoded in UTF-8
set updatetime=2000            " Millisecs idle before calling the CursorHold
set complete+=k,kspell         " Scan dictionaries for completion as well
set completeopt=menuone,longest,preview
set virtualedit+=block         " Block movement can go beyond end-of-line
set modelines=3                " Search the top and bottom 3 lines for modelines
set number                     " Show line numbers
set undofile                   " Persist undo history across sessions
set termguicolors              " Use guifg over ctermfg in true-color terminals
" Point the spell checker at my additional vocabulary words
let &spellfile=$VIMUSERRUNTIME . "/en.utf-8.add"

" Calculate the offset required to the cursor vertically centered on-screen.
execute 'set scrolloff='.(&lines-2)

" My default tab settings. YAIFA will attempt to change these based on the
" file I open, but if it can't determine the tab settings a file, fall back to
" these:
set softtabstop=2
set shiftwidth=2
set tabstop=4
set expandtab
" }}}

" {{{ Mappings

" Unbind G, and make gG go to the bottom of the file
noremap  gG G
onoremap gG G
map      G <Nop>

" Set semicolon to map leader, and comma repeats the last f/t/F/T
noremap  , ;
map      ; <Nop>
let mapleader = ";"

" Easily rapidly (un)indent in visual mode by immediately re-selecting
" afterwards
vnoremap < <gv
vnoremap > >gv

" Quick access to formatting motion
nnoremap Q gq
" Format the next paragraph, quick!
nnoremap gQ gqap

" TODO: fix dis
" if executable('fzf')
"   nmap   ZE :call fzf#run(fzf#wrap({'sink': 'e'}, 0))<CR>
"   nmap   ZS :call fzf#run(fzf#wrap({'sink': 'split'}))<CR>
"   nmap   ZV :call fzf#run(fzf#wrap({'sink': 'vnew'}))<CR>
" else
"   nmap   ZE :e <C-R>=expand("%:h")<CR>/
"   nmap   ZS :split <C-R>=expand("%:h")<CR>/
"   nmap   ZV :vnew <C-R>=expand("%:h")<CR>/
" endif
nnoremap ZE :e <C-R>=expand("%:h")<CR>/
nnoremap ZS :split <C-R>=expand("%:h")<CR>/
nnoremap ZV :vnew <C-R>=expand("%:h")<CR>/

" Sometimes you just need to move a character or two in insert mode. Don't
" make these a habit, though!
imap     <C-j> <Down>
imap     <C-k> <Up>
imap     <C-h> <Left>
imap     <C-l> <Right>

" Swap ` and ', making ' more precise (line & column) by default
nnoremap ` '
nnoremap ' `

" Toggle spell check quickly
nnoremap <silent> zP :set spell!<CR>

" Clear the screen of artifacts, and clear highlighting too.
map <silent> <c-l> <c-l>:nohlsearch<CR>

" Immediately select the recommended spelling correction of the word underneath
nnoremap zp 1z=

" Save the file only when the buffer has been modified.
nnoremap <silent> ZW :update<CR>

" Map gt to a motion that aligns text
nmap gt <Plug>(EasyAlign)
xmap gt <Plug>(EasyAlign)

" Map H and L to jump to the beginning or end of the line. H is smarter, in
" that it jumps between the first non-whitespace character or actual column 0.
noremap  <expr> H (col('.') == matchend(getline('.'), '^\s*')+1 ? '0' : '^')
map             L $

" Quickly toggle the asynchronous lint engine. It causes a bit of lag in the
" editor, so it's disabled by default.
nnoremap <silent> ZY :ALEToggle<CR>
" }}}

" {{{ Plugin configuration settings

" {{{{ Asynchronous Lint Engine (ALE)
" Point rubocop at my Vim-specific configuration file, that makes the
" real-time feedback more pleasant.
let g:ale_ruby_rubocop_options = "--config " . $VIMUSERRUNTIME . "/rubocop.yml"

" Disable ALE by default
let g:ale_enabled = 0
" }}}}

" }}}

runtime macros/matchit.vim " Extend % matching
runtime ftplugin/man.vim " :Man command

let g:PaperColor_Theme_Options = {
  \   'theme': {
  \     'default.dark': {
  \       'transparent_background': 1,
  \       'override' : {
  \         'color00' : ['#212529', '235'],
  \       }
  \     }
  \   }
  \ }
let g:hybrid_custom_term_colors = 1
colorscheme devolved

"highlight statusColNr gui=NONE guifg=#121212 guibg=#005f5f guisp=NONE
highlight link statusColNr Number
"highlight statusColNrPowerline gui=NONE guifg=#005f5f guibg=#1c1c1c guisp=NONE
"highlight statusColNcPowerline gui=NONE guifg=#005f5f guibg=#121212 guisp=NONE
"highlight statusFileName gui=NONE guifg=#ffaf00 guibg=#1c1c1c guisp=NONE
"highlight statusFileType gui=NONE guifg=#00afff guibg=#1c1c1c guisp=NONE
"highlight statusBranch gui=NONE guifg=#ff5f00 guibg=#1c1c1c guisp=NONE
"highlight statusFlag gui=NONE guifg=#ff00d7 guibg=#1c1c1c guisp=NONE

highlight link jinjaString String
