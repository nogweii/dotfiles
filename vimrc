"
" Maintainer: Colin Shea <colin@evaryont.me>
" Last Change: 6 June 2010
" vim: set fdm=marker:

" {{{ Settings
set      nocompatible                    " Enable vim-only options, those incompatible with classic Vi
set      nowrap                          " Don't wrap lines
set      noerrorbells                    " Disable any error bells
set      visualbell                      " Use a visual notification instead of beeping
set      t_vb          =                 " Modify the termcap entry: Disable the visual bell code
set      foldenable                      " Enable folding, at launch
set      foldmethod    =syntax           " Default to syntax based folds
set      foldminlines  =2                " Require at least 2 lines before actually closing a fold
set      hlsearch                        " Highlight search results
set      incsearch                       " Jump to the first match in real-time
set      ignorecase                      " Case insensitive search, by default.
set      smartcase                       " Become case-sensitive if there any capital letters
set      hidden                          " Allow changing buffers even with unsaved modifications
set      spell                           " Enable spell check
set      title                           " Modify the terminal title
set      number                          " Number lines
set      cursorline                      " Emphasize the current line the cursor is on
set      laststatus    =2                " Always show the status bar
set      ruler                           " Always show the position of the cursor
set      showcmd                         " Show incomplete commands in the status bar
set      autoindent                      " Automatically indent the next line
set      cindent                         " Smart automatic indent
set      linebreak                       " Don't wrap in the middle of a word
set      modeline                        " Let individual files specify settings
set      background    =dark             " Use dark colors over lighter ones
set      fileformat    =unix             " Prefer UNIX line endings
set      history       =50               " Remember the 50 most recent commands
set      wildmenu                        " Show completion matches in the status bar, above the command bar. And other Ex-mode goodies.
set      wildmode      =longest,full     " Expand the longest common match, then show the full menu
set      wildignore    =*.o,*.obj,*~     " What to ignore in wildmenu
set      backupdir     =/tmp             " Store backup files in /tmp
set      directory     =/tmp             " Store swap files in /tmp
set      backspace     =indent,eol,start " Smart backspace in insert mode
set      sidescroll    =1                " Scroll horizontally 1 column at a time
set      sidescrolloff =7                " Always show this at least this many columns
set      enc           =utf-8            " Use UTF-8 encoding
set      fenc          =utf-8
set      tenc          =utf-8
set      updatetime    =2000             " Milli secs of idle before calling the CursorHold autocmd
set      shortmess     =filnxtToOmIAr    " Use shorter messages in some ways
set      softtabstop=2
set      shiftwidth=2
set      tabstop=4
set      expandtab
set      complete+=k,kspell
set      completeopt=menuone,longest,preview
execute 'set scrolloff='.(&lines-2)
execute 'set list listchars=tab:' . nr2char(9655) . nr2char(160) . ',trail:' . nr2char(183)
" }}}

" {{{ Mappings
noremap  gG G
onoremap gG G
map      G <Nop>
map      zp 1z=
map      <silent> <c-l> <c-l>:nohlsearch<CR>
nmap     gp :.!xclip -out<CR>
map      <C-w>w :wn<CR>
vnoremap < <gv
vnoremap > >gv
nnoremap ` '
nnoremap ' `
nnoremap <C-j> 3j
nnoremap <C-k> 3k
vnoremap <C-j> 3j
vnoremap <C-k> 3k
map      H ^
map      L $
nmap     <S-Ins> :set paste<CR><S-Ins>:set nopaste<CR>
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>
nnoremap <silent> <C-n> :<C-U>SwitchToBuffer(v:count1)<CR>
nnoremap <silent> <C-p> :<C-U>SwitchToBuffer(-1*v:count1)<CR>
nnoremap Q gq
imap     <C-j> <Down>
imap     <C-k> <Up>
imap     <C-h> <Left>
imap     <C-l> <Right>
nnoremap <silent> <Leader>T :TlistToggle<CR>
nmap     <silent> ZW :update<CR>:TlistUpdate<CR>
map      <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
           \   . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
           \   . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
nmap     s cs
nmap     ZD :call CleanClose(0)<CR>
nmap     ZE :e <C-R>=expand("%:p:h")<CR>/
nmap     ZS :split <C-R>=expand("%:p:h")<CR>/
nmap     ZV :vnew <C-R>=expand("%:p:h")<CR>/
nnoremap gf gF
nnoremap <silent> gF :CommandT<CR>
" }}}

" {{{ Other Settings
colorscheme devolved " my personal blend of themes, including inkpot and calmar
syntax on " Enable syntax highlighting
filetype off " Turn off filetype before pathogen.
runtime macros/matchit.vim " Extend % matching
runtime ftplugin/man.vim " :Man command
" }}}

" {{{ Functions
" Append a vim modeline to the end of the file
function! AppendModeline()
  let save_cursor = getpos('.')
  let append = ' vim: set ts='.&tabstop.' sw='.&shiftwidth.' tw='.&textwidth.' syn='.&syntax.': '
  $put =substitute(&commentstring, '%s', append, '')
  call setpos('.', save_cursor)
endfunction

" Switch to the next buffer that is not help or taglist
function! SwitchToNextBuffer(incr)
    let current = bufnr("%")
    let last = bufnr("$")
    let new = current + a:incr
    while 1
        if new != 0 && bufexists(new) && !(getbufvar(new, "&filetype") == 'help') && !(getbufvar(new, "&filetype") == 'taglist') && !(getbufvar(new, "&filetype") == 'nerdtree')
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

function! CleanClose(tosave)
  if (a:tosave == 1)
    w!
  endif

  let todelbufNr = bufnr("%")
  let newbufNr = bufnr("#")

  if ((newbufNr != -1) && (newbufNr != todelbufNr) && buflisted(newbufNr))
    exe "b ".newbufNr
  else
    bnext
  endif

  if (bufnr("%") == todelbufNr)
    new
  endif

  exe "bd ".todelbufNr
  call Buftabs_show()
endfunction
" }}}

" {{{ Conditionals
if !has("ruby")
    let g:LustyJugglerSuppressRubyWarning = 1
    echo "The latest Vim 7.2 includes Ruby 1.9 support. Please upgrade."
endif

" if argc() > 1
"     " Avoid E173 - load the last buffer then switch back to the first
"     silent blast
"     silent bfirst
" endif
" }}}

" {{{ Let / Misc plugin configuration
let g:yaifa_max_lines                 =  1024    " The default is 16 times this many...whoa.
let g:showmarks_enable                =  0       " Don't enable showmarks automatically
let mapleader                         =  "G"     " Use 'G' as map leader, not the default '\'
let g:ruby_space_errors               =  1       " Enable space errors in Ruby files
let g:ruby_fold                       =  1       " Enable folding in Ruby files
let g:ruby_operators                  =  1       " Highlight ruby operators
let g:ruby_no_expensive               =  0       " Enable CPU expensive stuff
let g:rubycomplete_classes_in_global  =  1       " Add local buffer classes to the completion list
let g:Tlist_Auto_Highlight_Tag        =  1       " Track where I am in the file
let g:Tlist_Auto_Open                 =  1       " Open up on vim start
let g:Tlist_Enable_Fold_Column        =  0       " Don't show the fold column in the tag list window
let g:Tlist_File_Fold_Auto_Close      =  1       " Close folds for inactive files
let g:Tlist_Exit_OnlyWindow           =  1       " Exit vim when TagList is the only window open
let g:Tlist_Highlight_Tag_on_BufEnter =  1       " On BufEnter, highlight the correct tag
let g:Tlist_Sort_Type                 =  "order" " Sort by the order for which a tag appears, not alphabetically
let g:SuperTabDefaultCompletionType = "context"
let s:did_snips_mappings = 1
let snippets_dir = substitute(globpath(&rtp, 'snipmate-snippets/'), "\n", ',', 'g')
" Fuzzy finder: ignore stuff that can't be opened, and generated files
let g:fuzzy_ignore = "*.png;*.PNG;*.JPG;*.jpg;*.GIF;*.gif;vendor/**;coverage/**;tmp/**;rdoc/**"
let g:tcommentMapLeader1 = ''
let g:tcommentMapLeader2 = ''
let g:bufExplorerDefaultHelp=0       " Do not show default help.
let g:bufExplorerDetailedHelp=0      " Do not show detailed help.
" }}}

" {{{ Autocommands
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
"au BufWinLeave * mkview
"au BufWinEnter * silent loadview
au FileType * if &ft != 'help' | call GetSnippets(snippets_dir, &ft) | endif
" Delay calling GetSnippets 'til after vim has loaded all the plugins
au VimEnter * call GetSnippets(snippets_dir, '_') " Get global snippets
" Return to the last line you were editing in a file
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
" }}}

" {{{ Call commands
command! -nargs=1 SwitchToBuffer call SwitchToNextBuffer(<args>)
call pathogen#runtime_append_all_bundles()

" Turn filetype on *now*, with extra ftdetect paths added, so vim actually
" sees them!
filetype on
filetype plugin on
filetype indent on
" }}}

" " Namespace Ruby Debugger maps with 'rd'
" map <Leader>rdb :call g:RubyDebugger.toggle_breakpoint()<CR>
" map <Leader>rdv :call g:RubyDebugger.open_variables()<CR>
" map <Leader>rdm :call g:RubyDebugger.open_breakpoints()<CR>
" map <Leader>rdt :call g:RubyDebugger.open_frames()<CR>
" map <Leader>rds :call g:RubyDebugger.step()<CR>
" map <Leader>rdf :call g:RubyDebugger.finish()<CR>
" map <Leader>rdn :call g:RubyDebugger.next()<CR>
" map <Leader>rdc :call g:RubyDebugger.continue()<CR>
" map <Leader>rde :call g:RubyDebugger.exit()<CR>
" map <Leader>rdd :call g:RubyDebugger.remove_breakpoints()<CR>
