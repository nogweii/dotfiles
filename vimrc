" vim configuration file, from all over the web.
" Maintainer: Colin Shea <colin@evaryont.me>
" vim: set fdm=marker:

let g:showtab = 0
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
"set cursorline                 " Emphasize the current line the cursor is on
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
set backupdir=~/.vim/tmp//     " Store backup files in ~/.vim/tmp
set directory=~/.vim/tmp//     " Store swap files in ~/.vim/tmp
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
set viminfo+=n~/.vim/tmp/viminfo
set modelines=3                " Search the top and bottom 3 lines
execute 'set scrolloff='.(&lines-2)
"set list                       " Show certain chars for tabs & trailing chars
execute 'set listchars=trail:'.nr2char(183)
set number
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
nmap     zp 1z=
nmap     <silent> zP :set spell!<CR>
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
"nnoremap <silent> <Leader>T :TlistToggle<CR>
"nmap     <silent> ZW :update<CR>:TlistUpdate<CR>
nmap     <silent> ZW :update<CR>
nmap     ZD :call CleanClose(0)<CR>
if executable('fzf')
  nmap   ZE :call fzf#run(fzf#wrap({'sink': 'e'}, 0))<CR>
  nmap   ZS :call fzf#run(fzf#wrap({'sink': 'split'}))<CR>
  nmap   ZV :call fzf#run(fzf#wrap({'sink': 'vnew'}))<CR>
else
  nmap   ZE :e <C-R>=expand("%:h")<CR>/
  nmap   ZS :split <C-R>=expand("%:h")<CR>/
  nmap   ZV :vnew <C-R>=expand("%:h")<CR>/
endif
nmap     ZB :ls<CR>:b<Space>
nmap     <Leader>gt :GundoToggle<CR>
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
    set undodir=~/.vim/tmp/
    set undofile
endif
" }}}

" {{{ Let / Misc plugin configuration
let g:yaifa_max_lines=1024              " Only inspect the 1st KB of the file
let g:ruby_space_errors=1               " Enable space errors in Ruby files
let g:ruby_fold=1                       " Enable folding in Ruby files
let g:ruby_operators=1                  " Highlight ruby operators
let g:ruby_no_expensive=0               " Enable CPU expensive stuff
let g:rubycomplete_classes_in_global=1  " Add local classes to completion
let g:SuperTabDefaultCompletionType="context"
let g:SuperTabCrMapping=0
let g:delimitMate_expand_space=0
let g:delimitMate_expand_cr=0
" }}}

" {{{ Autocommands
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
"au BufWinLeave * mkview
"au BufWinEnter * silent loadview
"au FileType * if &ft != 'help' | call GetSnippets(snippets_dir, &ft) | endif
" Delay calling GetSnippets 'til after vim has loaded all the plugins
"au VimEnter * call GetSnippets(snippets_dir, '_') " Get global snippets
au BufReadPost *.pacmans setl nospell
autocmd FileType tmux setl nospell
" Restore the cursor's position every time you open a file.
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
" }}}

" {{{ Call commands
command! -nargs=1 SwitchToBuffer call SwitchToNextBuffer(<args>)
call pathogen#infect()
"command! MathBar call Sidebar("~/.vim/math_bar.txt")

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

autocmd FileType man setlocal textwidth=0 nospell
autocmd FileType diff setlocal nospell textwidth=0
autocmd FileType jproperties setlocal nospell

" Falls back to 'grepprg' when the Ack plugin is not installed
function! AckSearch()
    let string = input("Search: ")
    if exists(":Ack")
        execute ":Ack" string
    else
        execute ":grep" string
    endif
endfunction

nmap <silent> ZG :call AckSearch()<CR>
nmap ZA :A<CR>
nmap <leader>r :Include<CR>

" Return to the previous location after repeating a change
"nmap . .`[
" Disable vim-as-man-pager within vim (so :Man works)
let $MANPAGER = ''

autocmd FileType gitcommit normal :DiffGitCached
au FileType haskell set wildignore+=*.hi " Ignore more haskell compiled files
au BufReadPost /tmp/mutt-* setlocal nospell nolist ft=mail

" omnifunc autocompete settings
autocmd FileType python setl omnifunc=pythoncomplete#Complete
autocmd FileType javascript setl omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html setl omnifunc=htmlcomplete#CompleteTags
autocmd FileType css setl omnifunc=csscomplete#CompleteCSS
autocmd FileType xml setl omnifunc=xmlcomplete#CompleteTags
autocmd FileType php setl omnifunc=phpcomplete#CompletePHP
autocmd FileType ruby,eruby setl omnifunc=rubycomplete#Complete
autocmd FileType c,cpp,objc,objcpp setl omnifunc=ClangComplete

" python-specific settings
au FileType python setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4 smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class,with

" don't remove indents when typing a hash
inoremap # #

" Ctrl-A & Ctrl-X are awesome now!
nmap <Plug>SwapItFallbackIncrement <Plug>SpeedDatingUp
nmap <Plug>SwapItFallbackDecrement <Plug>SpeedDatingDown

" Strip trailing whitespace just before saving a file
"autocmd BufWritePre * :%s/\s\+$//e

au FileType man setlocal nolist

nnoremap zG zug
nnoremap zW zuw

au FileType sshconfig setl nospell

inoremap <expr> <C-e> pumvisible() ? "\<c-e>" : "\<c-o>A"
imap     <expr> <C-a> "\<c-o>H"

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Ack for the last search.
nnoremap <silent> <leader>? :execute "Ack! '" .  substitute(substitute(substitute(@/, "\\\\<", "\\\\b", ""), "\\\\>", "\\\\b", ""), "\\\\v", "", "") . "'"<CR>

nnoremap / /\v
vnoremap / /\v

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

au BufRead,BufNewFile *.wiki           setlocal filetype=mediawiki
au BufRead,BufNewFile *.wikipedia.org* setlocal filetype=mediawiki
au BufRead,BufNewFile *.wikibooks.org* setlocal filetype=mediawiki
au BufRead,BufNewFile *.wikimedia.org* setlocal filetype=mediawiki

" Disable a lot of gui stuffs.
set guioptions-=T
set guioptions-=M
set guioptions-=R
set guioptions-=m
set guioptions-=a
set guioptions-=L
set guioptions-=r

" I use git submodules to manage updates to my vim scripts
let g:loaded_getscriptPlugin = 1
" Don't need vimball loaded, either.
let g:loaded_vimballPlugin = 1

au BufRead ~/.yaourtrc setlocal filetype=sh
au FileType haskell setl nospell
autocmd BufWritePre */xmonad.hs setl syntax=haskell
autocmd FileType help setl nospell

au BufRead,BufNewFile ~/.local/share/zsh/* setlocal filetype=zsh
au BufRead,BufNewFile ~/dotfiles/share/zsh/* setlocal filetype=zsh
au BufRead,BufNewFile /usr/share/zsh/* setlocal filetype=zsh
au BufRead,BufNewFile ~/code/shinken-configs/*.cfg setlocal filetype=nagios
au BufRead /var/lib/robocode/*.java compiler javac | setlocal makeprg=/var/lib/robocode/robots/gui/compile.sh | nmap ZM :make %<CR>
au FileType tagbar setl nospell

au BufRead,BufNewFile Vagrantfile setlocal filetype=ruby
au FileType puppet,yaml setlocal nospell

au BufRead,BufNewFile */uzbl/config setlocal filetype=uzbl
au BufRead,BufNewFile */conkywx/conkywx.conf setlocal filetype=sh

let g:LargeFile = 1 " in megabytes, when the largefile plugin will activate
if executable('coffeetags')
  let g:tagbar_type_coffee = {
        \ 'ctagsbin' : 'coffeetags',
        \ 'ctagsargs' : '--include-vars',
        \ 'kinds' : [
        \ 'f:functions',
        \ 'o:object',
        \ ],
        \ 'sro' : ".",
        \ 'kind2scope' : {
        \ 'f' : 'object',
        \ 'o' : 'object',
        \ }
  \ }
else
  let g:tagbar_type_coffee = {
    \ 'ctagstype' : 'coffee',
    \ 'kinds'     : [
      \ 'c:classes',
      \ 'm:methods',
      \ 'f:functions',
      \ 'v:variables',
      \ 'f:fields',
    \ ]
  \ }
endif

" The return key doesn't activate the unindent in HTML files
autocmd FileType html setlocal indentkeys-=*<Return>
autocmd FileType markdown setlocal nocindent

" {{{ Customized version of folded text, derived from
" https://github.com/chrisbra/vim_dotfiles/blob/master/plugin/CustomFoldText.vim
fu! CustomFoldText()
    " get first non-blank line
    let fs = v:foldstart

    while getline(fs) =~ '^\s*$'
      let fs = nextnonblank(fs + 1)
    endwhile

    if fs > v:foldend
        let line = getline(v:foldstart)
    else
        let line = substitute(getline(fs), '\t', repeat(' ', &tabstop), 'g')
    endif

    " remove foldmarker from line
    let line = substitute(line,
                          matchstr(&l:cms, '^.\{-}\ze%s') .
                            '\s*' .
                            split(&l:fmr,',')[0] .
                            '\s*\d\+',
                          '', '')

    let width = winwidth(0) - &foldcolumn - (&number ? 8 : 0)
    let foldSize = 1 + v:foldend - v:foldstart
    let foldSizeStr = " " . foldSize . " lines "
    let foldLevelStr = '+'. v:folddashes
    let lineCount = line("$")
    let foldPercentage = printf("[%.1f", (foldSize*1.0)/lineCount*100) . "%] "
    let foldchar = matchstr(&fillchars, 'fold:\zs.')
    let foldStrSize = foldSizeStr . line . foldLevelStr . foldPercentage
    let expansionString = repeat(foldchar, width - strwidth(foldStrSize))
    return line . expansionString . foldSizeStr . foldPercentage . foldLevelStr
endf

set foldtext=CustomFoldText() " }}}

let g:markdown_fenced_languages = ['ruby', 'erb=eruby', 'python', 'bash=sh', 'zsh', 'javascript']
" I did install the powerline version of the font, so let's use it!
let g:airline_powerline_fonts = 1
" Nifty auto completion for some basic mustache syntax
let g:mustache_abbreviations = 1

set concealcursor=nc
set conceallevel=2

" magic maker map! will call :make but silently -- this will skip the 'Press
" ENTER to continue' message which is really nice. However, this is may leave
" the screen blank so we need to have vim send itself the Ctrl-L keybind to
" force it to redraw the screen (:redraw doesn't cut it). And because this is
" all silenced, it isn't included into the command history keeping it cleaner of
" repeated calls to make
nnoremap gm :silent :make<CR>:silent :exe "normal \<c-l>"<CR>

au BufRead,BufNewFile .eslintrc setlocal filetype=json

" Enable some additional syntax highlighting for javascript in the browser
let g:javascript_enable_domhtmlcss = 1

au BufRead,BufNewFile ~/dotfiles/gitconfig setlocal filetype=gitconfig

" A few dumb key bindings to test if the terminal I'm using supports a full
" variety of extra modifiers
map <M-u> :echo 'hi'<CR>
map <M-U> :echo 'bye'<CR>
map <C-u> :echo 'controlled response'<CR>
map <M-Enter> :echo 'alternate confirm'<CR>
map <C-Enter> :echo 'controlling question'<CR>
map <S-Enter> :echo 'shify person'<CR>
map <M-C-S-Enter> :echo 'why would you ever do this to me'<CR>
map <S-Home> :echo 'early riser'<CR>
map <C-End> :echo 'night owl'<CR>
map <S-F1> :echo 'big question'<CR>
map <M-F1> :echo 'alternate question'<CR>
map <C-F1> :echo 'questioning question'<CR>
map <S-C-F1> :echo 'questing knight'<CR>
map <S-M-F1> :echo 'magical girl function key!'<CR>
map <C-S-M-F1> :echo 'there be dragons here'<CR>
map <C-S-M-Down> :echo "don't look at me"<CR>

au BufRead,BufNewFile inventory/*.ini setlocal filetype=ansible_hosts
au FileType ansible_hosts setl nospell
" Better indenting & formatting
autocmd FileType markdown setlocal nocindent autoindent
au BufRead,BufNewFile ~/dotfiles/zsh/functions/* setlocal filetype=zsh
