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
Plug 'nathanielc/vim-tickscript'
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
Plug 'inkarkat/swapit' " formerly mjbrownie/swapit
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

" Integrate with external tools
Plug 'KabbAmine/zeavim.vim'
Plug 'junegunn/fzf'

" Super advanced completion!
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-syntax'
Plug 'ncm2/ncm2-ultisnips'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'Shougo/neco-syntax'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

Plug 'wincent/command-t', {
  \   'do': 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make'
  \ }

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

augroup super_tab_completion
  autocmd!

  autocmd BufEnter  *  call ncm2#enable_for_buffer()

  " Set mappings for only supported filetypes.
  autocmd FileType * call LanguageClient_Maps()
  " Run gofmt and goimports on save
  autocmd BufWritePre *.go :call LanguageClient#textDocument_formatting_sync()
  autocmd CursorHold * call LanguageClient_Hovering()

  autocmd User LanguageClientStarted setlocal signcolumn=yes
  autocmd User LanguageClientStopped setlocal signcolumn=auto
  autocmd User LanguageClientStarted :lcd b:LanguageClient_projectRoot
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

" directly set the path to the system python3, so it avoids using the
" virtualenv
let g:python3_host_prog = "/usr/bin/python3"

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
map <silent> <c-l> <c-l>:nohlsearch<CR>:call LanguageClient#clearDocumentHighlight()<CR>

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

" {{{{ Language server configuration
" Configure the language servers
let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.local/cargo/bin/rls'],
    \ 'ruby': ['~/.local/ruby/bin/solargraph', 'stdio'],
    \ 'go': ['~/.local/go/bin/gopls'],
    \ 'python': ['~/.local/pypi/bin/pyls'],
    \ 'dockerfile': ['~/.local/node/bin/docker-langserver', '--stdio'],
    \ 'sh': ['bash-language-server', 'start'],
    \ 'javascript': ['~/.local/node/bin/javascript-typescript-stdio'],
    \ 'vim': ['~/.local/node/bin/vim-language-server', '--stdio'],
    \ 'css': ['~/.local/node/bin/css-languageserver', '--stdio'],
    \ 'scss': ['~/.local/node/bin/css-languageserver', '--stdio'],
    \ 'html': ['~/.local/node/bin/html-languageserver', '--stdio']
    \ }
" Only send text updates to the language server every this seconds
let g:LanguageClient_changeThrottle = 0.5
" Don't use fzf for the context menu
let g:LanguageClient_fzfContextMenu = 0
let g:LanguageClient_hoverPreview = 'Never'
let g:LanguageClient_useVirtualText = 0

let g:LanguageClient_settingsPath = $VIMUSERRUNTIME . 'languageservers.json'
"let g:LanguageClient_loadSettings = ~/.local/share/nvim/languageclient.log

" }}}}

" {{{{ Disable mappings from various plugins
let g:zv_disable_mapping = 1
let g:endwise_no_mappings = 1
" }}}}

" }}}

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

function LanguageClient_Maps()
  if has_key(g:LanguageClient_serverCommands, &filetype)
    nnoremap <silent> <F5> :call LanguageClient_contextMenu()<CR>
    nnoremap <silent> gd   :call LanguageClient#textDocument_definition()<CR>
    nnoremap <silent> Z=   :call LanguageClient#textDocument_formatting_sync()<CR>
    nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
    nnoremap <silent> ZT   :call LanguageClient#textDocument_documentSymbol()<CR>
  endif
endfunction

function LanguageClient_Hovering()
  if g:LanguageClient_serverStatusMessage ==# ''
    " No language server is running (not installed?), don't try to get the
    " hover text
    return
  endif
  if has_key(g:LanguageClient_serverCommands, &filetype)
    call LanguageClient#textDocument_hover()
  endif
endfunction

" }}}
