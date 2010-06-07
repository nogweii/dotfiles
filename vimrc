"
" Maintainer: Colin Shea <colin@evaryont.me>
" Last Change: 6 June 2010
" vim: set fdm=marker:

" {{{ Settings
set      nocompatible
set      nowrap
set      noerrorbells
set      nofoldenable
set      incsearch
set      ignorecase
set      smartcase
set      hidden
set      spell
set      wildmenu
set      title
set      visualbell
set      nu
set      cursorline
set      cindent
set      ruler
set      showcmd
set      autoindent
set      expandtab
set      linebreak
set      modeline
set      hlsearch
set      foldnestmax   =3
set      background    =dark
set      fileformat    =unix
set      history       =50
set      wildmode      =longest,full
set      backupdir     =~/.vim/tmp,~/tmp,/var/tmp,/tmp
set      backspace     =indent,eol,start
set      wildignore    =*.o,*.obj,*~
set      shiftwidth    =4
set      sidescroll    =1
set      sidescrolloff =7
set      softtabstop   =4
set      tabstop       =4
set      enc           =utf-8
set      fenc          =utf-8
set      tenc          =utf-8
set      updatetime    =2000
set      laststatus    =2
execute 'set directory='.&backupdir
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
     \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
     \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
 " }}}

" {{{ Other Settings
colorscheme devolved " my personal blend of themes, including inkpot and calmar
syntax on " Enable syntax highlighting
filetype on
filetype plugin on
filetype indent on
runtime macros/matchit.vim " Extend % matching
" }}}

" {{{ Functions
function! AppendModeline()
  let save_cursor = getpos('.')
  let append = ' vim: set ts='.&tabstop.' sw='.&shiftwidth.' tw='.&textwidth.' syn='.&syntax.': '
  $put =substitute(&commentstring, '%s', append, '')
  call setpos('.', save_cursor)
endfunction

function! SwitchToNextBuffer(incr)
    let help_buffer = (&filetype == 'help')
    let taglist_buffer = (&filetype == 'taglist')
    let current = bufnr("%")
    let last = bufnr("$")
    let new = current + a:incr
    while 1
        if new != 0 && bufexists(new) && ((getbufvar(new, "&filetype") == 'help') == help_buffer) && ((getbufvar(new, "&filetype") == 'taglist') == taglist_buffer)
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
" }}}

" {{{ Autocmds
" Return to the last line you were editing in a file
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
" }}}

" {{{ Conditionals
if !has("ruby")
    let g:LustyJugglerSuppressRubyWarning = 1
    echo "The latest Vim 7.2 includes Ruby 1.9 support. Please upgrade."
endif

if argc() > 1
    " Avoid E173 - load the last buffer then switch back to the first
    silent blast
    silent bfirst
endif
" }}}

" {{{ Let / Misc plugin configuration
let g:yaifa_max_lines = 1024 " The default is 16 times this many...whoa.
let g:showmarks_enable = 0
let mapleader="G"
let ruby_space_errors = 1
let ruby_fold = 1
let g:Tlist_Auto_Highlight_Tag = 1 " Track where I am in the file
let g:Tlist_Auto_Update = 1 " Update on saves
let g:Tlist_Auto_Open = 1 " Open up on vim start
let g:Tlist_Enable_Fold_Column = 0
let g:Tlist_File_Fold_Auto_Close = 1 " Close folds for inactive files
let g:Tlist_Exit_OnlyWindow = 1
let g:Tlist_GainFocus_on_ToggleOpen = 1
let g:Tlist_Highlight_Tag_on_BufEnter = 1
let g:Tlist_Show_Menu = 0
let g:Tlist_Use_Right_Window = 1
let g:Tlist_Show_One_File = 1
" }}}

" {{{ Call commands
command! -nargs=1 SwitchToBuffer call SwitchToNextBuffer(<args>)
call pathogen#runtime_append_all_bundles()
" }}}
