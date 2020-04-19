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

" A package of language support files, like syntax highlighting
Plug 'sheerun/vim-polyglot'
" Add TICKscript (Influx Kapacitor 1.x) syntax
Plug 'nathanielc/vim-tickscript'

" Automatically configure various editor settings in a standard way
Plug 'editorconfig/editorconfig-vim'

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

" Briefly highlight whatever I yank
Plug 'machakann/vim-highlightedyank'
" Color each level of nested pairs a different color
Plug 'alok/rainbow_parentheses.vim', {'branch': 'fix-spell'} " use this branch to include https://github.com/alok/rainbow_parentheses.vim/commit/3d1152441c21a03fa9d6302c700e0cb7eb80469c, fixing spell check
Plug 'andrewradev/sideways.vim'
Plug 'inkarkat/swapit'
Plug 'chr4/sslsecure.vim'
Plug 'tweekmonster/startuptime.vim'

" Various color schemes I have tried...
" Plug 'morhetz/gruvbox'
" Plug 'nanotech/jellybeans.vim'
" Plug 'dracula/vim'
" Plug 'jnurmine/zenburn'
" Plug 'tpope/vim-vividchalk'
" Plug 'jonathanfilip/vim-lucius'
" Plug 'junegunn/seoul256.vim'
" Plug 'tomasr/molokai'
" Plug 'chriskempson/base16-vim'
" Plug 'nlknguyen/papercolor-theme'
" Plug 'w0ng/vim-hybrid'
" Plug 'kristijanhusak/vim-hybrid-material'

" Integrate with external tools
Plug 'KabbAmine/zeavim.vim'
Plug 'junegunn/fzf'

" A plugin that allows me to tab complete a snippet of code with just a keyword
Plug 'SirVer/ultisnips'
" ..and a collection of said snippets for a variety of languages
Plug 'honza/vim-snippets'

Plug 'wincent/command-t', {
  \   'do': 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make'
  \ }

call plug#end() " }}}

" {{{ Autocommand groups
augroup vimrc_folding
  autocmd!
  au BufReadPost $MYVIMRC setlocal foldmethod=marker
augroup END

augroup recalculate_scrolloffset
  autocmd!
  au VimResized * execute 'set scrolloff='.(&lines-2)
augroup END

augroup nvim_default_tweaking
  autocmd!
" au BufReadPost * if &keywordprg ==? ':Man' | set keywordprg= | endif
  au FileType css,scss setlocal iskeyword+=-
augroup END

augroup manual_docset_definitions
  autocmd!
  au BufReadPost $MYVIMRC let b:manualDocset = 'vim'
  au BufReadPost ansible.cfg let b:manualDocset = 'ansible'
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
set completeopt=noinsert,menuone,noselect
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

" Create the backup directory if it doesn't exist already
set backupdir=$XDG_DATA_HOME/nvim/backup
if !isdirectory($XDG_DATA_HOME . "/nvim/backup")
  execute "silent! !mkdir " . $XDG_DATA_HOME . "/nvim/backup"
endif

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
"nnoremap ZE <Plug>(CommandT)
nmap <silent> ZE <Plug>(CommandT)
"nnoremap ZS :split <C-R>=expand("%:h")<CR>/
"nnoremap ZV :vnew <C-R>=expand("%:h")<CR>/

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

" Launch Zeal from vim easily: Press K! (unless you specify a custom keyword
" program to search for)
nnoremap <silent> K :call <SID>SmartZeal(0)<CR>
vnoremap <silent> K :call <SID>SmartZeal(1)<CR>
nmap ZK <Plug>ZVKeyDocset

" Use <TAB> to select the popup menu
"inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
"inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

"inoremap <silent> <expr> <CR> ncm2_ultisnips#expand_or("\<CR>", 'n')
"imap <silent> <CR> <CR><Plug>DiscretionaryEnd
imap <silent> <expr> <CR> pumvisible() ? ncm2_ultisnips#expand_or("\<CR>", 'n') : "\<CR>\<Plug>DiscretionaryEnd"

" }}}

" {{{ Plugin configuration settings

" {{{{ Asynchronous Lint Engine (ALE)
" Point rubocop at my Vim-specific configuration file, that makes the
" real-time feedback more pleasant.
let g:ale_ruby_rubocop_options = "--config " . $VIMUSERRUNTIME . "/rubocop.yml"

" Disable ALE by default
let g:ale_enabled = 0
" }}}}

" {{{{ Zeal/Vim integration
let g:zv_file_types = {
  \   'yaml.ansible': 'ansible',
  \   'scss': 'css,sass',
  \   'html': 'html,css,javascript',
  \   'python': 'python_3',
  \   '\v^(G|g)ulpfile\.js': 'gulp,javascript,nodejs',
  \   '\v^(md|mdown|mkd|mkdn)$': 'markdown',
  \   '\v^(G|g)runt\.': 'grunt,javascript,nodejs',
  \   '.htaccess': 'apache_http_server',
  \   'ansible.cfg': 'ansible',
  \ }
" }}}}

" {{{{ Disable mappings from various plugins
let g:zv_disable_mapping = 1
let g:endwise_no_mappings = 1
" }}}}

" {{{{ Rainbow Parentheses configuration
augroup rainbow_parentheses
  autocmd!
  au VimEnter * call rainbow_parentheses#activate()
augroup END

" Manually define the colors of the pairs. This normally derived automatically
" from the color scheme, but that breaks with mine.
let s:para_colors = map([
      \ 'Yellow',
      \ 'Magenta',
      \ 'Green',
      \ 'Cyan',
      \ 'Brown',
      \ 'Blue',
      \ 'DarkGreen',
      \ 'Red',
      \ ], '[v:val, v:val]')
let g:rainbow#max_level = len(s:para_colors)
let g:rainbow#colors = { 'dark': s:para_colors, 'light': s:para_colors }
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}'], ['<', '>']]
" }}}}

" }}}

let g:hybrid_custom_term_colors = 1
colorscheme devolved

highlight link statusColNr Number
highlight link jinjaString String

" Don't consider acronyms/abbreviations at least 3 long as spelling errors.
" Includes a trailing 's' at the end, and any numbers as part of the acronym.
syn match NoSpellAcronym '\<\(\u\|\d\)\{3,}s\?\>' contains=@NoSpell
" Don't consider URL-like things as spelling errors
syn match NoSpellUrl '\w\+:\/\/[^[:space:]]\+' contains=@NoSpell

" {{{ Supporting functions

function <SID>SmartZeal(is_visual)
  if &keywordprg ==? ':Man'
    if a:is_visual
      ZeavimV
    else
      Zeavim
    endif
  else
    normal! K
  endif
endfunction

" }}}

augroup RestoreSavedCursor
  au!
  " when I edit a file, restore the cursor to the saved position
  autocmd BufReadPost *
        \ if expand("<afile>:p:h") !=? $TEMP |
        \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \ let RestoreSavedCursor_line = line("'\"") |
        \ let b:doopenfold = 1 |
        \ if (foldlevel(RestoreSavedCursor_line) > foldlevel(RestoreSavedCursor_line - 1)) |
        \ let RestoreSavedCursor_line = RestoreSavedCursor_line - 1 |
        \ let b:doopenfold = 2 |
        \ endif |
        \ exe RestoreSavedCursor_line |
        \ endif |
        \ endif

  " postpone using "zv" until after reading the modeline
  autocmd BufWinEnter *
        \ if exists("b:doopenfold") |
        \ exe "normal zv" |
        \ if(b:doopenfold > 1) |
        \ exe "+".1 |
        \ endif |
        \ unlet b:doopenfold |
        \ endif
augroup END
