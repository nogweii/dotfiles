" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Change: Mon Jun 06 10:00  2011 W
" Note:		This file is a part of Automatic Tex Plugin for Vim.

"
" {{{ Load Once
if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1
" }}}

" Status Line:
function! ATPBibStatus() "{{{
    return substitute(expand("%"),"___","","g")
endfunction
setlocal statusline=%{ATPBibStatus()}
" }}}

" Maps:
" {{{ MAPPINGS 
if !exists("no_plugin_maps") && !exists("no_atp_bibsearch_maps")
    map <buffer> c :call BibChoose()<CR>
    map <buffer> y :call BibChoose()<CR>
    map <buffer> p :call BibChoose()<CR>
    map <buffer> q :hide<CR>
    command! -buffer -nargs=* BibChoose 	:call BibChoose(<f-args>)
endif
" }}}

" Functions:
function! BibChoose(...)" {{{
    if a:0 == 0
	let which	= input("Which entry? ( <Number><reg name><Enter>, <Number><Enter> or <Enter> for none) ")
    else
	let which	= a:1
    endif
    let g:which = which
    if which == ""
	return
    endif
    if which =~ '^\d*$' 
	let start	= stridx(b:ListOfBibKeys[which],'{')+1
	let choice	= substitute(strpart(b:ListOfBibKeys[which], start), ',\s*$', '', '')
	let g:choice	= choice

	" Goto right buffer
	let winbufnr = bufwinnr(b:BufNr)
	if winbufnr != -1
	    exe "normal ".winbufnr."w"
	else
	    if bufexist(b:BufNr)
		exe "normal buffer ".winbufnr
	    else
		echohl WarningMsg 
		echo "Buffer was deleted"
		echohl None
		return
	    endif
	endif

	let LineNr 	= line(".")
	let ColNr 	= col(".") 
	call setline(LineNr, strpart(getline(LineNr), 0, ColNr) . choice . strpart(getline(LineNr), ColNr))
	call cursor(LineNr, len(strpart(getline(LineNr), 0, ColNr) . choice)+1)
	return
    elseif which =~ '^\d*\(\a\|+\| . "*" .\)$'
	    let letter=substitute(which,'\d','','g')
	    let g:letter = letter
	    let which=substitute(which,'\a\|+\|' . "*",'','g')
	    let start=stridx(b:ListOfBibKeys[which],'{')+1
	    let choice=substitute(strpart(b:ListOfBibKeys[which], start), ',', '', '')
	    if letter == 'a'
		let @a=choice
	    elseif letter == 'b'
		let @b=choice
	    elseif letter == 'c'
		let @c=choice
	    elseif letter == 'd'
		let @d=choice
	    elseif letter == 'e'
		let @e=choice
	    elseif letter == 'f'
		let @f=choice
	    elseif letter == 'g'
		let @g=choice
	    elseif letter == 'h'
		let @h=choice
	    elseif letter == 'i'
		let @i=choice
	    elseif letter == 'j'
		let @j=choice
	    elseif letter == 'k'
		let @k=choice
	    elseif letter == 'l'
		let @l=choice
	    elseif letter == 'm'
		let @m=choice
	    elseif letter == 'n'
		let @n=choice
	    elseif letter == 'o'
		let @o=choice
	    elseif letter == 'p'
		let @p=choice
	    elseif letter == 'q'
		let @q=choice
	    elseif letter == 'r'
		let @r=choice
	    elseif letter == 's'
		let @s=choice
	    elseif letter == 't'
		let @t=choice
	    elseif letter == 'u'
		let @u=choice
	    elseif letter == 'v'
		let @v=choice
	    elseif letter == 'w'
		let @w=choice
	    elseif letter == 'x'
		let @x=choice
	    elseif letter == 'y'
		let @y=choice
	    elseif letter == 'z'
		let @z=choice
	    elseif letter == '*'
		let @-=choice
	    elseif letter == '+'
		let @+=choice
	    elseif letter == '-'
		let @@=choice
	    endif
	    echohl WarningMsg | echomsg "[ATP:] choice yanekd to the register '" . letter . "'" | echohl None
    endif
endfunction "}}}
" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
