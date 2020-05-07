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

" A nice and quick directory viewer (the '-' keybinding)
Plug 'justinmk/vim-dirvish'
" Launch the file manager or new terminal easily from within vim
Plug 'justinmk/vim-gtfo'

" Tim Pope series of plugins. Quite a prolific vimscript author!
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-fugitive'

" Smartly insert the other (), [], '', "", <>
Plug 'raimondi/delimitmate'
" Scan the opened file, guess various indentation rules from it
Plug 'raimondi/yaifa'

" Briefly highlight whatever I yank
Plug 'machakann/vim-highlightedyank'
" Color each level of nested pairs a different color
Plug 'alok/rainbow_parentheses.vim', {'branch': 'fix-spell'} " use this branch to include https://github.com/alok/rainbow_parentheses.vim/commit/3d1152441c21a03fa9d6302c700e0cb7eb80469c, fixing spell check

" Custom text objects to interact with arguments/parameters in a list, or a
" few other column/array like arrangements of text
Plug 'andrewradev/sideways.vim'

" Swap keywords in a list, using <C-a> and <C-x>
Plug 'inkarkat/swapit'

" Look for OpenSSL protocol and ciphers that are known to be insecure, and
" highlight them
Plug 'chr4/sslsecure.vim'

" Use :StartupTime to get an average of 10 runs of `nvim --startuptime` and
" present a nice display of what's taking so long startup. Also, see the shell
" alias 'nvim-startup-benchmark'
Plug 'tweekmonster/startuptime.vim'

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

" Easily put a character/pair around some text. Sandwich a word between
" parentheses!
Plug 'machakann/vim-sandwich'

Plug 'joshdick/onedark.vim'
Plug 'KeitaNakamura/neodark.vim'

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

nnoremap <silent> ZE <Plug>(CommandT)
nnoremap <silent> ZB <Plug>(CommandTBuffer)

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

" Launch Zeal from vim easily: Press K! (unless you specify a custom keyword
" program to search for)
nnoremap <silent> K :call <SID>SmartZeal(0)<CR>
vnoremap <silent> K :call <SID>SmartZeal(1)<CR>
nmap ZK <Plug>ZVKeyDocset

" Use <TAB> to select the popup menu
"inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
"inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" imap <silent> <expr> <CR> pumvisible() ? ncm2_ultisnips#expand_or("\<CR>", 'n') : "\<CR>\<Plug>DiscretionaryEnd"
imap <silent> <expr> <CR> "\<CR>\<Plug>DiscretionaryEnd"

" Make gf smarter by default, freeing up gF to be a quick alias to vim-gtfo
nnoremap gf gF
nnoremap <silent> gF :<c-u>call gtfo#open#file(getcwd())<cr>

nnoremap <C-n> :bnext<CR>
nnoremap <C-p> :bprevious<CR>

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

" Switch sandwich to using surround.vim's key bindings, which I'm very used
" to, while still taking advantage of the extra functionality
runtime macros/sandwich/keymap/surround.vim



" }}}

" {{{ Color scheme settings

let g:onedark_terminal_italics = 1
" These colors I've overridden to draw from the PatternFly palette
let g:onedark_color_overrides = {
      \ "green":          { "gui": "#5ba352", "cterm": "71",  "_pf": "green 400"  },
      \ "purple":         { "gui": "#8476d1", "cterm": "104", "_pf": "purple 400" },
      \ "dark_red":       { "gui": "#c9190b", "cterm": "88",  "_pf": "red 300"    },
      \ "red":            { "gui": "#a30000", "cterm": "124", "_pf": "red 200"    },
      \ "yellow":         { "gui": "#f4c145", "cterm": "221", "_pf": "gold 300"   },
      \ "dark_yellow":    { "gui": "#f0ab00", "cterm": "214", "_pf": "gold 400"   },
      \ "blue":           { "gui": "#2b9af3", "cterm": "75",  "_pf": "blue 300"   },
      \ "cyan":           { "gui": "#009596", "cterm": "30",  "_pf": "cyan 300"   },
      \ "white":          { "gui": "#f5f5f5", "cterm": "255", "_pf": "black 150"  },
      \ "black":          { "gui": "#151515", "cterm": "233", "_pf": "black 900"  },
      \ "comment_grey":   { "gui": "#4f5255", "cterm": "239", "_pf": "black 700"  },
      \ "cursor_grey":    { "gui": "#212427", "cterm": "235", "_pf": "black 850"  },
      \ "gutter_fg_grey": { "gui": "#3c3f42", "cterm": "237", "_pf": "black 800"  },
      \ "visual_grey":    { "gui": "#737679", "cterm": "243", "_pf": "black 600"  },
      \ "menu_grey":      { "gui": "#737679", "cterm": "243", "_pf": "black 600"  },
      \ "special_grey":   { "gui": "#8a8d90", "cterm": "245", "_pf": "black 500"  },
      \ "vertsplit":      { "gui": "#0f280d", "cterm": "235", "_pf": "green 700"  },
      \}

colorscheme onedark

let s:colors = onedark#GetColors()
call onedark#set_highlight('statusColNr', { "fg": s:colors["yellow"], "bg": s:colors["gutter_fg_grey"] })
call onedark#set_highlight('statusColNrPowerline', {"fg": s:colors["gutter_fg_grey"], "bg": s:colors["cursor_grey"] })
call onedark#set_highlight('statusColNcPowerline', { "fg": s:colors["comment_grey"] })
call onedark#extend_highlight('WildMenu', { "bg": s:colors["green"] })
call onedark#extend_highlight('PmenuSel', { "bg": s:colors["green"] })
highlight link jinjaString String

" Don't consider acronyms/abbreviations at least 3 long as spelling errors.
" Includes a trailing 's' at the end, and any numbers as part of the acronym.
syn match NoSpellAcronym '\<\(\u\|\d\)\{3,}s\?\>' contains=@NoSpell
" Don't consider URL-like things as spelling errors
syn match NoSpellUrl '\w\+:\/\/[^[:space:]]\+' contains=@NoSpell

" }}}

" {{{ Supporting functions

" Only run Zeal if the keywordprg is set to ":Man", which it will be
" automatically if it's not set to anything else by another language-specific
" plugin. If a docset is set, however, always use Zeal.
function <SID>SmartZeal(is_visual)
  if get(b:, 'manualDocset', 1)
    if a:is_visual
      call zeavim#SearchFor('', '', 'v')
    else
      call zeavim#SearchFor('', expand('<cword>'))
    endif
  elseif &keywordprg ==? ':Man'
    if a:is_visual
      call zeavim#SearchFor('', '', 'v')
    else
      call zeavim#SearchFor('', expand('<cword>'))
    endif
  else
    normal! K
  endif
endfunction

" }}}
