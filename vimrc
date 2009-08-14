"
" Maintainer: Colin Shea <evaryont@gmx.us>
" Last Change: 2009 Feb 19

set nocompatible " This is Vim, not Vi!

" Never, ever start as evim. Seriously, I don't like it at all!
if v:progname =~? "evi"
	quitall
end

" Look & Feel
set t_Co=256 " I have 256 colors in my terminal, darn it!
set nowrap " Disable line wraps </3
let g:inkpot_black_background = 1
"colorscheme inkpot " Thanks, omp!
set background=dark
"colorscheme calmar256-dark
colorscheme devolved

" Misc options
set incsearch " Find the moment I start searching, real-time
set ignorecase " Case insensitive searching...
set smartcase " ...except when I search for an uppercase letter
syntax on " Enable syntax highlighting
set fileformat=unix " What sequence to use for end-of-line (\n in this case)
set hidden " Modified buffers are saved in the bg when exiting
set spell " Enable spell checking, mostly this only affects comments.
noremap gG G
onoremap gG G
map G <Nop>
let mapleader="G"
set history=1000 " Remember more stuff, vim!
runtime macros/matchit.vim " Extend % matching
set wildmenu " Nice menu for tab completion
set wildmode=longest,full " Personal choice about matching style, see ':h wildmode'
set title " Update the title with the VIM messages
set backupdir=~/.vim/tmp,~/tmp,/var/tmp,/tmp " Centralize location of backup
set directory=~/.vim/tmp,~/tmp,/var/tmp,/tmp " files (e.g. .swp files)
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
nnoremap <C-j> 3<C-e>
nnoremap <C-k> 3<C-y>
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
  " Keep append split on 2 lines to keep vim from trying to parse the line as
  let append = ' vim' " ..a modeline, when we're trying to build one instead.
  let append = append.': set ts='.&tabstop.' sw='.&shiftwidth.' tw='.&textwidth.' syn='.&syntax.': '
  $put =substitute(&commentstring, '%s', append, '')
  call setpos('.', save_cursor)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

let ruby_space_errors = 1
let ruby_fold = 1

let g:Tlist_Auto_Highlight_Tag = 1
let g:Tlist_Auto_Update = 1
let g:Tlist_File_Fold_Auto_Close = 1
let g:Tlist_Enable_Fold_Column = 0
let g:Tlist_Exit_OnlyWindow = 1
let g:Tlist_GainFocus_on_ToggleOpen = 1
let g:Tlist_Highlight_Tag_on_BufEnter = 1
let g:Tlist_Show_Menu = 0
let g:Tlist_Use_Right_Window = 1

nnoremap <silent> <Leader>tl :TlistToggle<CR>
map ZW :w<CR>

set enc=utf-8
set fenc=utf-8
set termencoding=utf-8

" when using list, keep tabs at their full width and display `arrows':
" (Character 187 is a right double-chevron, and 183 a mid-dot.)
" 160 is a non-breaking space.
"execute 'set list listchars=tab:' . nr2char(187) . nr2char(160) . ',trail:' . nr2char(183)
execute 'set list listchars=tab:' . nr2char(160) . nr2char(160) . ',trail:' . nr2char(183)
"execute 'set list listchars=trail:' . nr2char(183)

function! SwitchToNextBuffer(incr)
	let help_buffer = (&filetype == 'help')
	let current = bufnr("%")
	let last = bufnr("$")
	let new = current + a:incr
	while 1
		if new != 0 && bufexists(new) && ((getbufvar(new, "&filetype") == 'help') == help_buffer)
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

" IRB
autocmd FileType irb inoremap <buffer> <silent> <CR> <Esc>:<C-u>ruby VIM::Buffer.current[VIM::Buffer.current.line_number] = VIM::Buffer.current[VIM::Buffer.current.line_number] + " #=> " +eval(VIM::Buffer.current[VIM::Buffer.current.line_number]).inspect<CR>o
nnoremap <leader>irb :<C-u>below new<CR>:setfiletype irb<CR>:set syntax=ruby<CR>:set buftype=nofile<CR>:set bufhidden=delete<CR>i

" Move around in insert mode
imap <C-j> <Down>
imap <C-k> <Up>
imap <C-h> <Left>
imap <C-l> <Right>
