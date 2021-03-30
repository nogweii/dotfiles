" NeoVim config file, created by Evaryont. <hello@evaryont.me>
"
" A lot of this is stuff I've run into over the years, from various forums
" and my own experimentation. Some of it is archaic and silly, others neat
" or interesting. I hope you agree, and that you may find something that
" intrigues you as well.
" -- Happy vimming! :smile

" Get the directory that contains init.vim. (this is probably ~/.config/nvim/ )
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

" Bug fix https://github.com/neovim/neovim/issues/12587
" and see the readme: https://github.com/antoinemadec/FixCursorHold.nvim/blob/master/README.md
Plug 'antoinemadec/FixCursorHold.nvim'

" file type icons powered by nerdfont
Plug 'ryanoasis/vim-devicons'

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

" Automatically jump to the project's root directory
Plug 'airblade/vim-rooter'

" Persist vim buffers, etc across executions, automatically
Plug 'thaerkh/vim-workspace'

" A fancy 'tabline' that shows all of the buffers & tabs
Plug 'bagrat/vim-buffet'

" Automatically build and maintain a tags file
Plug 'ludovicchabant/vim-gutentags'

" Easily launch unit tests from within vim
Plug 'vim-test/vim-test'

" Popup menu and automatic completion suggestions
Plug 'prabirshrestha/asyncomplete.vim'

" Language Server Protocol support
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'thomasfaingnaert/vim-lsp-ultisnips'

" sources for asyncomplete.vim
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/asyncomplete-ultisnips.vim'
Plug 'prabirshrestha/asyncomplete-emoji.vim'
Plug 'prabirshrestha/asyncomplete-buffer.vim'
Plug 'prabirshrestha/asyncomplete-tags.vim'

" Vim is a wiki and a Zettelkasten method
Plug 'vimwiki/vimwiki'
Plug 'michal-h21/vim-zettel'


call plug#end() " }}}

" {{{ Autocommand groups
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

" Various minor changes in neovim UX to smooth it out
augroup smooth_out_vim
  au!

  " Detect filetype after saving a file without a known one
  au BufWritePost * nested
        \ if &l:filetype ==# '' || exists('b:ftdetect')
        \ |   unlet! b:ftdetect
        \ |   filetype detect
        \ | endif

  " Resize the windows to be equal whenever vim is resized
  au VimResized * tabdo wincmd =

  " After opening a file, move the cursor back to where it last was
  au BufReadPost * if &ft !~# 'commit' && ! &diff &&
                 \      line("'\"") >= 1 && line("'\"") <= line("$")
                 \ |   execute 'normal! g`"zvzz'
                 \ | endif

  " Disable the swap & undo files for various ephemeral files
  au BufWritePre /tmp/*,COMMIT_EDITMSG,MERGE_MSG setlocal noundofile noswapfile

  " Turn off spell check in the terminal
  au TermEnter * setlocal nospell

  autocmd BufWinEnter,WinEnter * if &buftype == 'quickfix' | setlocal nospell | endif

augroup END
" }}}

" {{{ NeoVim settings
set nowrap                     " Don't wrap lines
set noerrorbells               " Disable any error bells
set laststatus=2               " Always show the status bar
set hidden                     " Allow changing buffers even with modifications
set spell                      " Enable spell check
set title                      " Modify the terminal title
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
set updatetime=100             " Millisecs idle before calling the CursorHold
set complete+=k,kspell         " Scan dictionaries for completion as well
set completeopt=noinsert,menuone,noselect,preview
set virtualedit+=block         " Block movement can go beyond end-of-line
set modelines=3                " Search the top and bottom 3 lines for modelines
set number                     " Show line numbers
set undofile                   " Persist undo history across sessions
set termguicolors              " Use guifg over ctermfg in true-color terminals
set sessionoptions-=blank      " Don't save empty windows in the session
set sessionoptions-=buffers    " Don't save hidden buffers into the session
set sessionoptions-=help       " Ignore the help buffer for sessions
set sessionoptions-=options    " Don't save any vim options (this list)
set sessionoptions-=globals    " Ignore any g:-variables
set sessionoptions+=localoptions " Include buffer local overrides
set sessionoptions+=tabpages   " This session is for all tabs, not individual ones
set formatoptions+=r           " Add comment syntax to new lines in insert mode
set formatoptions+=o           " Automatically add comment syntax after o/O
set shortmess+=F               " Don't print a message when opening a file
set foldlevel=5                " Only fold sections deeper than this level automatically
set foldlevelstart=5           " Only fold sections deeper than this level automatically
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

set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case\ --follow
set grepformat=%f:%l:%c:%m

" directly set the path to the system python3, so it avoids using the
" virtualenv
let g:python3_host_prog = "/usr/bin/python3"

" Where do I stick various things that make vim a bit more like an IDE? (this
" is inside each project's folder)
let s:vim_ide_folder = '.vim-stuff'
execute "set tags+=" . s:vim_ide_folder . "/tags"
let g:gutentags_ctags_tagfile = s:vim_ide_folder . "/tags"
let g:gutentags_ctags_executable_ruby = 'ripper-tags'

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

nmap <silent> ZE <Plug>(CommandT)
nmap <silent> ZB <Plug>(CommandTBuffer)

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

" imap <silent> <expr> <CR> pumvisible() ? ncm2_ultisnips#expand_or("\<CR>", 'n') : "\<CR>\<Plug>DiscretionaryEnd"
imap <silent> <expr> <CR> "\<CR>\<Plug>DiscretionaryEnd"

" Make gf smarter by default, freeing up gF to be a quick alias to vim-gtfo
nnoremap gf gF
nnoremap <silent> gF :<c-u>call gtfo#open#file(getcwd())<cr>

nnoremap <silent> <C-n> :bnext<CR>
nnoremap <silent> <C-p> :bprevious<CR>

nnoremap <silent> ZD :Bw<CR>

" Launch a test suite from wihtin vim
nmap <silent> <leader>tt :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ts :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>te :TestVisit<CR>

" Easily get out of insert mode in the terminal
tmap <C-s> <C-\><C-n>

nnoremap <silent> ZG :call <SID>GrepPrompt()<CR>
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

" {{{{ Project root directory configuration
" Switch to the file's current directory if we're not in a found project
let g:rooter_change_directory_for_non_project_files = 'current'
" Change only the local buffer's directory, not the entire vim
let g:rooter_cd_cmd = 'lcd'
" Be silent when changing directories
let g:rooter_silent_chdir = 1

" XXX: rooter sets `b:rootDir` to the absolute path of the folder
" }}}}

" {{{{ Workspace configuration

" Don't need to have workspace also manage my undo settings, I've got it
" (globally)
let g:workspace_persist_undo_history = 0

" When calling vim with an argument, open those files as buffers, not tabs
let g:workspace_create_new_tabs = 0

" Don't automatically save files
let g:workspace_autosave = 0
let g:workspace_autosave_untrailspaces = 0

let g:workspace_session_name = s:vim_ide_folder . "/Session.vim"
" }}}}

" {{{{ Bufferline tweaks
let g:bufferline_excludes = ['\[vimfiler\]', 'Command-T']
" }}}}

" {{{{ Buffet tweaks
let g:buffet_prefix = 'buffetHi'

let g:buffet_use_devicons = 1

let g:buffet_tab_icon = "\uf9e8" " material design icon, tab
let g:buffet_left_trunc_icon = "\uf0a8"
let g:buffet_right_trunc_icon = "\uf0a9"
let g:buffet_noseparator = ""
let g:buffet_separator = ""
"let g:buffet_separator = "\u2502" " box drawing characters, light vertical
let g:buffet_modified_icon = " \uf692 " " material design icon, save/floppy

function! g:BuffetSetCustomColors()
  hi! BuffetTab           cterm=NONE guifg=#f5f5f5 ctermfg=255 guibg=#2b9af3 ctermbg=5
  hi! BuffetTrunc         cterm=NONE guifg=#f5f5f5 ctermfg=255 guibg=#151515 ctermbg=233
  hi! BuffetBuffer        cterm=NONE guifg=#f5f5f5 ctermfg=255 guibg=#151515 ctermbg=233
  hi! BuffetCurrentBuffer cterm=NONE guifg=#009596 ctermfg=30  guibg=#151515 ctermbg=233
endfunction
" }}}}

" {{{{ vim-test config
let test#strategy = "mine"
" let test#neovim#term_position = "botright"
" }}}}

" {{{{ vimwiki config
let g:vimwiki_list = [{'path': '~/wiki/', 'syntax': 'markdown', 'ext': '.md'}]
let g:vimwiki_global_ext = 0
" }}}}

" Switch sandwich to using surround.vim's key bindings, which I'm very used
" to, while still taking advantage of the extra functionality
runtime macros/sandwich/keymap/surround.vim

let g:delimitMate_tab2exit = 0

" }}}

" {{{ Color scheme settings

try
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
  call onedark#set_highlight('statusFileName', { "fg": s:colors["cyan"], "bg": s:colors["cursor_grey"] })
  call onedark#set_highlight('statusFileType', { "fg": s:colors["dark_yellow"], "bg": s:colors["cursor_grey"] })
  call onedark#set_highlight('statusFlag', { "fg": s:colors["purple"], "bg": s:colors["cursor_grey"] })
  call onedark#extend_highlight('WildMenu', { "bg": s:colors["green"] })
  call onedark#extend_highlight('PmenuSel', { "bg": s:colors["green"] })
catch /E185:/
  echo "Onedark colorscheme is missing. Run nvim-plugs alias to install it"
endtry
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

function! <SID>GrepPrompt()
  let l:searchterm = input("Project Search: ")
  silent! exe 'grep! ' . l:searchterm
  if len(getqflist())
    botright copen
    redraw!
  else
    cclose
    redraw!
    echohl WarningMsg | echo "No search results for " . l:searchterm | echohl None
  endif
endfunction


" }}}
