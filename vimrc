"
" Maintainer: Colin Shea <evaryont@gmx.us>
" Last Change: 2009 Feb 19

set nocompatible " This is Vim, not Vi!

" Never, ever start as evim. Seriously, I don't like it at all!
if v:progname =~? "evi"
	quitall
end

" gG => go to end of file
" G  => leader key
noremap gG G
onoremap gG G
map G <Nop>
let mapleader="G"

" Look & Feel
"set t_Co=256 " I have 256 colors in my terminal, darn it!
set nowrap " Disable line wraps </3
let g:inkpot_black_background = 1
"colorscheme inkpot " Thanks, omp!
set background=dark
"colorscheme calmar256-dark
colorscheme devolved " my personal blend of themes, including inkpot and calmar

" Misc options
set incsearch " Find the moment I start searching, real-time
set ignorecase " Case insensitive searching...
set smartcase " ...except when I search for an uppercase letter
syntax on " Enable syntax highlighting
set fileformat=unix " What sequence to use for end-of-line (\n in this case)
set hidden " Modified buffers are stored in the bg when closing
set spell " Enable spell checking, mostly this only affects comments.
set history=50 " Remember more stuff, vim!
runtime macros/matchit.vim " Extend % matching
set wildmenu " Nice menu for tab completion
set wildmode=longest,full " Personal choice about matching style, see ':h wildmode'
set title " Update the title with the VIM messages
" Centralize location of backup files (e.g. .swp files)
set backupdir=~/.vim/tmp,~/tmp,/var/tmp,/tmp
" Set directory to always be the same as &backupdir
execute 'set directory='.&backupdir
set visualbell t_vb= " Enable visual bell, but use nothing for visual bell
set noerrorbells  " Disable error bells in general

" Window management mappings
" Save the window and go to the next one
map <C-w>w :wn<CR>

" Code formatting
vnoremap < <gv
vnoremap > >gv

" Movement boosters
" Swap ` & ' - same thing, but ` goes both column & row, not just the row (')
nnoremap ` '
nnoremap ' `
" Scroll the viewport faster
nnoremap <C-j> 3j
nnoremap <C-k> 3k
" jump to the beginning/end of the line
map H ^
map L $

if !exists("g:vimMode")
	let g:vimMode = "vim"
end
if g:vimMode == "notes"
	let g:treeExplLocalRoot = "/home/colin/Dropbox/Notes"
	execute "cd " . g:treeExplLocalRoot
	au VimEnter * exe 'VSTreeExplore' . g:treeExplLocalRoot | wincmd w
	" call s:TreeExplorer(1, '')
	" call VSTreeExplore()
end
" echo g:vimMode

let g:treeExplVertical = 1
let g:treeExplWinSize = 25

"autocmd BufRead,BufNewFile *.mkd *.md set ai formatoptions=tcroqn2 comments=n:>

nmap <S-Ins> :set paste<CR><S-Ins>:set nopaste<CR>

map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
	\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
	\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Empty mouse, I don't like it interfering when I try to do a simple
" selection.
set mouse=

set modeline " Enable dynamic configuration per-file with special syntax
" Append modeline after last line in buffer.
" Use substitute() (not printf()) to handle '%%s' modeline in LaTeX files.
function! AppendModeline()
  let save_cursor = getpos('.')
  let append = ' vim: set ts='.&tabstop.' sw='.&shiftwidth.' tw='.&textwidth.' syn='.&syntax.': '
  $put =substitute(&commentstring, '%s', append, '')
  call setpos('.', save_cursor)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

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

nnoremap <silent> <Leader>T :TlistToggle<CR>
nmap <silent> ZW :update<CR>:TlistUpdate<CR>

" Set UTF-8 as the default
set  enc=utf-8
set fenc=utf-8
set tenc=utf-8

" when using list, keep tabs at their full width and display `arrows':
" (Character 187 is a right double-chevron, and 183 a mid-dot.)
" 160 is a non-breaking space.
execute 'set list listchars=tab:' . nr2char(9655) . nr2char(160) . ',trail:' . nr2char(183)

" Like the classic <C-n> / <C-p>, but skip help buffers
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
command! -nargs=1 SwitchToBuffer call SwitchToNextBuffer(<args>)
nnoremap <silent> <C-n> :<C-U>SwitchToBuffer(v:count1)<CR>
nnoremap <silent> <C-p> :<C-U>SwitchToBuffer(-1*v:count1)<CR>

" Restore the older Q[motion] binding
" Formats whatever motion moves over
nnoremap Q gq

" Move around in insert mode
imap <C-j> <Down>
imap <C-k> <Up>
imap <C-h> <Left>
imap <C-l> <Right>

set updatetime=2000 " ms to wait before writing swap & CursorHold autocmd
set hlsearch " Show all search results

" *much* shorter message than Lusty's
if !has("ruby")
	let g:LustyJugglerSuppressRubyWarning = 1
	echo "Ruby not found! Please compile vim with +ruby support"
	echo "---------------------------"
	echo "Yes it does work with ruby 1.9+, there is a patch for that"
endif

function! Twitter()
  if !exists("g:twitvim_login")
	  call inputsave()
	  let username = input("Your Twitter user name: ")
	  let password = inputsecret("Your Twitter password: ")
	  call inputrestore()
	  let g:twitvim_login = username.":".password
  endif
  FriendsTwitter
endfunction
command! Twitter call Twitter()

" Don't highlight the search results, alert the user to it
map <Leader>/ :nohlsearch \| :echo "Not highlighting results"<CR>

set nu
set cursorline
set cindent
if &lines > 1
	set laststatus=2
else
	set laststatus=0
endif
set ruler showcmd
set backspace=indent,eol,start

set wildignore=*.o,*.obj,*~
set autoindent expandtab
set foldnestmax=3
set linebreak nofoldenable
execute 'set scrolloff='.(&lines-2)
set shiftwidth=4
set sidescroll=1
set sidescrolloff=7
set softtabstop=4 tabstop=4

autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

if argc() > 1
	" Avoid E173 - load the last buffer then switch back to the first
	silent blast
	silent bfirst
endif

function! OutlineFoldExpr(lnum)
    if getline(a:lnum) =~ '^$'
        return -1
"    elseif indent(a:lnum) > indent(a:lnum+1)
"       return '<'.indent(a:lnum)
    elseif indent(a:lnum) < indent(a:lnum+1)
       return '>'.indent(a:lnum+1)
    else
        return indent(a:lnum)
    endif
endfunction

function! OutlineFold()
    setlocal foldenable
    setlocal foldmethod=expr
    setlocal foldexpr=OutlineFoldExpr(v:lnum)
    setlocal foldtext=getline(v:foldstart)
    setlocal fillchars=fold:\ "(there's a space after that \)
endfunction
command! OutlineFold call OutlineFold()

" Haxxors: `gp` to add the X11 clipboard directly to the file, at the cursor
nmap gp :.!xclip -out<CR>

"augroup git-wip
"  autocmd!
"  autocmd BufWritePost * :silent !git wip save "  wip: %:p:t" --editor -- "%"
"augroup END

map <silent> <c-l> <c-l>:nohlsearch<CR>

" Run pathogen
call pathogen#runtime_append_all_bundles()
