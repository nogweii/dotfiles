" vim configuration file, from all over the web.
" Maintainer: Colin Shea <colin@evaryont.me>
" vim: set fdm=marker:

" {{{ Settings
set nocompatible               " Enable vim-only options
set nowrap                     " Don't wrap lines
set noerrorbells               " Disable any error bells
set visualbell                 " Use a visual notification instead of beeping
set t_vb=                      " Disable the visual bell termcap
set foldenable                 " Enable folding, at launch
set foldmethod=syntax          " Default to syntax based folds
set foldminlines=2             " Require at least 2 lines before closing a fold
set hlsearch                   " Highlight search results
set incsearch                  " Jump to the first match in real-time
set ignorecase                 " Case insensitive search, by default.
set smartcase                  " Case-sensitive if there any capital letters
set hidden                     " Allow changing buffers even with modifications
set spell                      " Enable spell check
set title                      " Modify the terminal title
set number                     " Number lines
set cursorline                 " Emphasize the current line the cursor is on
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
set backupdir=/tmp             " Store backup files in /tmp
set directory=/tmp             " Store swap files in /tmp
set backspace=indent,eol,start " Smart backspace in insert mode
set sidescroll=1               " Scroll horizontally 1 column at a time
set sidescrolloff =7           " Always show this at least this many columns
set enc=utf-8                  " Use UTF-8 encoding
set fenc=utf-8
set tenc=utf-8
set updatetime=2000            " Millisecs idle before calling the CursorHold
set shortmess =filnxtToOmIAr   " Use shorter messages in some ways
set softtabstop=2
set shiftwidth=2
set tabstop=4
set expandtab
set complete+=k,kspell         " Scan dictionaries for completion as well
set completeopt=menuone,longest,preview
set virtualedit+=block         " Block movement can go beyond end-of-line
execute 'set scrolloff='.(&lines-2)
set list                       " Show certain chars for newline, tab, etc
execute 'set listchars=tab:'.nr2char(9655).nr2char(160).',trail:'.nr2char(183)
" statusline setup
set statusline=%f "tail of the filename

"display a warning if fileformat isn't unix
set statusline+=%#warningmsg#
set statusline+=%{&ff!='unix'?'['.&ff.']':''}
set statusline+=%*

"display a warning if file encoding isn't UTF-8
set statusline+=%#warningmsg#
set statusline+=%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.']':''}
set statusline+=%*

set statusline+=%h "help file flag
set statusline+=%y "filetype
set statusline+=%r "read only flag
set statusline+=%m "modified flag

"display a warning if &et is wrong, or we have mixed-indenting
set statusline+=%#error#
set statusline+=%{StatuslineTabWarning()}
set statusline+=%*

set statusline+=%{StatuslineTrailingSpaceWarning()}

set statusline+=%{StatuslineLongLineWarning()}

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

"display a warning if &paste is set
set statusline+=%#error#
set statusline+=%{&paste?'[paste]':''}
set statusline+=%*

set statusline+=%= "left/right separator
set statusline+=%{StatuslineCurrentHighlight()}\ \  " current highlight
set statusline+=%c, "cursor column
set statusline+=%l/%L "cursor line/total lines
set statusline+=\ %P "percent through file
set textwidth=80
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
noremap  <expr> H (col('.') == matchend(getline('.'), '^\s*')+1 ? '0' : '^')
map      L $
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>
nnoremap <silent> <C-n> :<C-U>SwitchToBuffer(v:count1)<CR>
nnoremap <silent> <C-p> :<C-U>SwitchToBuffer(-1*v:count1)<CR>
" Quick access to formatting
nnoremap Q gq
" Format the next paragraph, quick!
nmap     gQ gqap
imap     <C-j> <Down>
imap     <C-k> <Up>
imap     <C-h> <Left>
imap     <C-l> <Right>
nnoremap <silent> <Leader>T :TlistToggle<CR>
nmap     <silent> ZW :update<CR>:TlistUpdate<CR>
map      <F10> :echo "hi<".synIDattr(synID(line("."),col("."),1),"name").'>'
     \   . ' trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
     \   . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
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
  let append =  ' vim: set ts='.&tabstop.' sw='.&shiftwidth
  let append += ' tw='.&textwidth.' syn='.&syntax.': '
  $put =substitute(&commentstring, '%s', append, '')
  call setpos('.', save_cursor)
endfunction

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

" Close the current buffer in a clean manner
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

function! Sidebar(filepath)
    execute ":TlistClose"
    execute ":NERDTreeClose"
    execute ":10vpslit"
    execute ":edit ".a:filepath
    setlocal nonumber
endfunction

" recalculate the trailing whitespace warning when idle, and after saving
autocmd cursorhold,bufwritepost * unlet! b:statusline_trailing_space_warning

" return '[\s]' if trailing white space is detected
" return '' otherwise
function! StatuslineTrailingSpaceWarning()
    if !exists("b:statusline_trailing_space_warning")

        if !&modifiable
            let b:statusline_trailing_space_warning = ''
            return b:statusline_trailing_space_warning
        endif

        if search('\s\+$', 'nw') != 0
            let b:statusline_trailing_space_warning = '[\s]'
        else
            let b:statusline_trailing_space_warning = ''
        endif
    endif
    return b:statusline_trailing_space_warning
endfunction

" return the syntax highlight group under the cursor ''
function! StatuslineCurrentHighlight()
    let name = synIDattr(synID(line('.'),col('.'),1),'name')
    if name == ''
        return ''
    else
        return '[' . name . ']'
    endif
endfunction

" recalculate the tab warning flag when idle and after writing
autocmd cursorhold,bufwritepost * unlet! b:statusline_tab_warning

" return '[&et]' if &et is set wrong
" return '[mixed-indenting]' if spaces and tabs are used to indent
" return an empty string if everything is fine
function! StatuslineTabWarning()
    if !exists("b:statusline_tab_warning")
        let b:statusline_tab_warning = ''

        if !&modifiable
            return b:statusline_tab_warning
        endif

        let tabs = search('^\t', 'nw') != 0

        "find spaces that aren't used as alignment in the first indent column
        let spaces = search('^ \{' . &ts . ',}[^\t]', 'nw') != 0

        if tabs && spaces
            let b:statusline_tab_warning = '[mixed-indenting]'
        elseif (spaces && !&et) || (tabs && &et)
            let b:statusline_tab_warning = '[&et]'
        endif
    endif
    return b:statusline_tab_warning
endfunction

" recalculate the long line warning when idle and after saving
autocmd cursorhold,bufwritepost * unlet! b:statusline_long_line_warning

" return a warning for "long lines" where "long" is either &textwidth or 80 (if
" no &textwidth is set)
"
" return '' if no long lines
" return '[#x,my,$z] if long lines are found, were x is the number of long
" lines, y is the median length of the long lines and z is the length of the
" longest line
function! StatuslineLongLineWarning()
    if !exists("b:statusline_long_line_warning")

        if !&modifiable || &tw < 1
            let b:statusline_long_line_warning = ''
            return b:statusline_long_line_warning
        endif

        let long_line_lens = s:LongLines()

        if len(long_line_lens) > 0
            let b:statusline_long_line_warning = "[>".len(long_line_lens)."]"
            " Only highlight the column if there are any long lines
            if v:version >= 703
                execute "set colorcolumn=".&tw
            endif
        else
            let b:statusline_long_line_warning = ""
            if v:version >= 703
                set colorcolumn=""
            endif
        endif
    endif
    return b:statusline_long_line_warning
endfunction

" return a list containing the lengths of the long lines in this buffer
function! s:LongLines()
    let threshold = (&tw ? &tw : 80)
    let spaces = repeat(" ", &ts)

    let long_line_lens = []

    let i = 1
    while i <= line("$")
        let len = strlen(substitute(getline(i), '\t', spaces, 'g'))
        if len > threshold
            call add(long_line_lens, len)
        endif
        let i += 1
    endwhile

    return long_line_lens
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

" Vim 7.3 only settings.
if v:version >= 703
    " undo settings
    set undodir=~/.vim/undofiles
    set undofile
endif
" }}}

" {{{ Let / Misc plugin configuration
let g:yaifa_max_lines=1024              " Only inspect the 1st KB
let g:showmarks_enable=0                " Don't enable showmarks automatically
let mapleader="G"                       " Use 'G' as map leader instead of '\'
let g:ruby_space_errors=1               " Enable space errors in Ruby files
let g:ruby_fold=1                       " Enable folding in Ruby files
let g:ruby_operators=1                  " Highlight ruby operators
let g:ruby_no_expensive=0               " Enable CPU expensive stuff
let g:rubycomplete_classes_in_global=1  " Add local classes to completion
let g:Tlist_Auto_Highlight_Tag=1        " Track where I am in the file
let g:Tlist_Auto_Open=1                 " Open up on vim start
let g:Tlist_Enable_Fold_Column=0        " Don't show the fold column in TagList
let g:Tlist_File_Fold_Auto_Close=1      " Close folds for inactive files
let g:Tlist_Exit_OnlyWindow=1           " Exit vim when TagList is the only one
let g:Tlist_Highlight_Tag_on_BufEnter=1 " On BufEnter, highlight the correct tag
let g:Tlist_Sort_Type="order"           " Sort by location in code
let g:SuperTabDefaultCompletionType="context"
let snippets_dir = substitute(globpath(&rtp,'snipmate-snippets/'),"\n",',','g')
" Fuzzy finder: ignore stuff that can't be opened, and generated files
let g:fuzzy_ignore  = "*.png;*.PNG;*.JPG;*.jpg;*.GIF;*.gif;vendor/**;"
let g:fuzzy_ignore += "coverage/**;tmp/**;rdoc/**"
let g:tcommentMapLeader1 = ''
let g:tcommentMapLeader2 = ''
let g:bufExplorerDefaultHelp=0       " Do not show default help.
let g:bufExplorerDetailedHelp=0      " Do not show detailed help.
let g:SuperTabCrMapping=0
let g:delimitMate_expand_space=0
let g:delimitMate_expand_cr=0
let g:syntastic_enable_signs=1
" }}}

" {{{ Autocommands
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
"au BufWinLeave * mkview
"au BufWinEnter * silent loadview
au FileType * if &ft != 'help' | call GetSnippets(snippets_dir, &ft) | endif
" Delay calling GetSnippets 'til after vim has loaded all the plugins
au VimEnter * call GetSnippets(snippets_dir, '_') " Get global snippets
au BufReadPost *.pacmans set nospell
" Return to the last line you were editing in a file
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
" }}}

" {{{ Call commands
command! -nargs=1 SwitchToBuffer call SwitchToNextBuffer(<args>)
call pathogen#runtime_append_all_bundles()
command! MathBar call Sidebar("~/.vim/math_bar.txt")

" Turn filetype on *now*, with extra ftdetect paths added, so vim actually
" sees them!
filetype on
filetype plugin on
filetype indent on
" }}}

" visual search mappings
function! s:VSetSearch()
    let temp = @@
    norm! gvy
    let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
    let @@ = temp
endfunction
vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR>
vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR>

autocmd FileType man set textwidth=0
autocmd FileType diff set nospell
