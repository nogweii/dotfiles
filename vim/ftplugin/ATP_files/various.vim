" Author:      Marcin Szamotulski	
" Descriptiion:	These are various editting tools used in ATP.
" Note:	       This file is a part of Automatic Tex Plugin for Vim.
" Language:    tex
" Last Change: Thu Jun 09 10:00  2011 W

let s:sourced 	= exists("s:sourced") ? 1 : 0

" FUNCTIONS: (source once)
if !s:sourced || g:atp_reload_functions "{{{
" This is the wrap selection function.
" {{{ WrapSelection
function! s:WrapSelection(wrapper,...)

    let l:end_wrapper 	= ( a:0 >= 1 ? a:1 : '}' )
    let l:cursor_pos	= ( a:0 >= 2 ? a:2 : 'end' )
    let l:new_line	= ( a:0 >= 3 ? a:3 : 0 )

"     let b:new_line=l:new_line
"     let b:cursor_pos=l:cursor_pos
"     let b:end_wrapper=l:end_wrapper

    let l:begin=getpos("'<")
    " todo: if and on 'ą' we should go one character further! (this is
    " a multibyte character)
    let l:end=getpos("'>")
    let l:pos_save=getpos(".")

    " hack for that:
    let l:pos=deepcopy(l:end)
    keepjumps call setpos(".",l:end)
    execute 'normal l'
    let l:pos_new=getpos(".")
    if l:pos_new[2]-l:pos[2] > 1
	let l:end[2]+=l:pos_new[2]-l:pos[2]-1
    endif

    let l:begin_line=getline(l:begin[1])
    let l:end_line=getline(l:end[1])

    let b:begin=l:begin[1]
    let b:end=l:end[1]

    " ToDo: this doesn't work yet!
    let l:add_indent='    '
    if l:begin[1] != l:end[1]
	let l:bbegin_line=strpart(l:begin_line,0,l:begin[2]-1)
	let l:ebegin_line=strpart(l:begin_line,l:begin[2]-1)

	" DEBUG
	let b:bbegin_line=l:bbegin_line
	let b:ebegin_line=l:ebegin_line

	let l:bend_line=strpart(l:end_line,0,l:end[2])
	let l:eend_line=strpart(l:end_line,l:end[2])

	if l:new_line == 0
	    " inline
" 	    let b:debug=0
	    let l:begin_line=l:bbegin_line.a:wrapper.l:ebegin_line
	    let l:end_line=l:bend_line.l:end_wrapper.l:eend_line
	    call setline(l:begin[1],l:begin_line)
	    call setline(l:end[1],l:end_line)
	    let l:end[2]+=len(l:end_wrapper)
	else
" 	    let b:debug=1
	    " in seprate lines
	    let l:indent=atplib#CopyIndentation(l:begin_line)
	    if l:bbegin_line !~ '^\s*$'
		let l:begin_choice=1
		call setline(l:begin[1],l:bbegin_line)
		call append(l:begin[1],l:indent.a:wrapper) " THERE IS AN ISSUE HERE!
		call append(copy(l:begin[1])+1,l:indent.substitute(l:ebegin_line,'^\s*','',''))
		let l:end[1]+=2
	    elseif l:bbegin_line =~ '^\s\+$'
		let l:begin_choice=2
		call append(l:begin[1]-1,l:indent.a:wrapper)
		call append(l:begin[1],l:begin_line.l:ebegin_line)
		let l:end[1]+=2
	    else
		let l:begin_choice=3
		call append(copy(l:begin[1])-1,l:indent.a:wrapper)
		let l:end[1]+=1
	    endif
	    if l:eend_line !~ '^\s*$'
		let l:end_choice=4
		call setline(l:end[1],l:bend_line)
		call append(l:end[1],l:indent.l:end_wrapper)
		call append(copy(l:end[1])+1,l:indent.substitute(l:eend_line,'^\s*','',''))
	    else
		let l:end_choice=5
		call append(l:end[1],l:indent.l:end_wrapper)
	    endif
	    if (l:end[1] - l:begin[1]) >= 0
		if l:begin_choice == 1
		    let i=2
		elseif l:begin_choice == 2
		    let i=2
		elseif l:begin_choice == 3 
		    let i=1
		endif
		if l:end_choice == 5 
		    let j=l:end[1]-l:begin[1]+1
		else
		    let j=l:end[1]-l:begin[1]+1
		endif
		while i < j
		    " Adding indentation doesn't work in this simple way here?
		    " but the result is ok.
		    call setline(l:begin[1]+i,l:indent.l:add_indent.getline(l:begin[1]+i))
		    let i+=1
		endwhile
	    endif
	    let l:end[1]+=2
	    let l:end[2]=1
	endif
    else
	let l:begin_l=strpart(l:begin_line,0,l:begin[2]-1)
	let l:middle_l=strpart(l:begin_line,l:begin[2]-1,l:end[2]-l:begin[2]+1)
	let l:end_l=strpart(l:begin_line,l:end[2])
	if l:new_line == 0
	    " inline
	    let l:line=l:begin_l.a:wrapper.l:middle_l.l:end_wrapper.l:end_l
	    call setline(l:begin[1],l:line)
	    let l:end[2]+=len(a:wrapper)+1
	else
	    " in seprate lines
	    let b:begin_l=l:begin_l
	    let b:middle_l=l:middle_l
	    let b:end_l=l:end_l

	    let l:indent=atplib#CopyIndentation(l:begin_line)

	    if l:begin_l =~ '\S' 
		call setline(l:begin[1],l:begin_l)
		call append(copy(l:begin[1]),l:indent.a:wrapper)
		call append(copy(l:begin[1])+1,l:indent.l:add_indent.l:middle_l)
		call append(copy(l:begin[1])+2,l:indent.l:end_wrapper)
		if substitute(l:end_l,'^\s*','','') =~ '\S'
		    call append(copy(l:begin[1])+3,l:indent.substitute(l:end_l,'^\s*','',''))
		endif
	    else
		call setline(copy(l:begin[1]),l:indent.a:wrapper)
		call append(copy(l:begin[1]),l:indent.l:add_indent.l:middle_l)
		call append(copy(l:begin[1])+1,l:indent.l:end_wrapper)
		if substitute(l:end_l,'^\s*','','') =~ '\S'
		    call append(copy(l:begin[1])+2,l:indent.substitute(l:end_l,'^\s*','',''))
		endif
	    endif
	endif
    endif
    if l:cursor_pos == "end"
	let l:end[2]+=len(l:end_wrapper)-1
	call setpos(".",l:end)
    elseif l:cursor_pos =~ '\d\+'
	let l:pos=l:begin
	let l:pos[2]+=l:cursor_pos
	call setpos(".",l:pos)
    elseif l:cursor_pos == "current"
	keepjumps call setpos(".",l:pos_save)
    elseif l:cursor_pos == "begin"
	let l:begin[2]+=len(a:wrapper)-1
	keepjumps call setpos(".",l:begin)
    endif
endfunction
"}}}
"{{{ Inteligent Wrap Selection 
" This function selects the correct font wrapper for math/text environment.
" the rest of arguments are the same as for WrapSelection (and are passed to
" WrapSelection function)
" a:text_wrapper	= [ 'begin_text_wrapper', 'end_text_wrapper' ] 
" a:math_wrapper	= [ 'begin_math_wrapper', 'end_math_wrapper' ] 
" if end_(math\|text)_wrapper is not given '}' is used (but neverthe less both
" arguments must be lists).
function! s:InteligentWrapSelection(text_wrapper, math_wrapper, ...)

    let cursor_pos	= ( a:0 >= 1 ? a:2 : 'end' )
    let new_line	= ( a:0 >= 2 ? a:3 : 0 )

    let MathZones = copy(g:atp_MathZones)
    let pattern		= '^texMathZone[VWX]'
    if b:atp_TexFlavor == 'plaintex'
	call add(MathZones, 'texMathZoneY')
	let pattern	= '^texMathZone[VWXY]'
    endif

    " select the correct wrapper

    let MathZone	= get(filter(map(synstack(line("."),max([1,col(".")-1])),"synIDattr(v:val,'name')"),"v:val=~pattern"),0,"")
    if MathZone	=~ '^texMathZone[VWY]'
	let step 	= 2
    elseif MathZone == 'texMathZoneX'
	let step 	= 1
    else
	let step	= 0
    endif

    " Note: in visual mode col(".") returns always the column starting position of
    " the visual area, thus it is enough to check the begining (if we stand on
    " $:\(:\[:$$ use text wrapper). 
    if !empty(MathZone) && col(".") > step && atplib#CheckSyntaxGroups(MathZones, line("."), max([1, col(".")-step]))
	let begin_wrapper 	= a:math_wrapper[0]
	let end_wrapper 	= get(a:math_wrapper,1, '}')
    else
	let begin_wrapper	= a:text_wrapper[0]
	let end_wrapper		= get(a:text_wrapper,1, '}')
    endif

    " if the wrapper is empty return
    " useful for wrappers which are valid only in one mode.
    if begin_wrapper == ""
	return
    endif

    call s:WrapSelection(begin_wrapper, end_wrapper, cursor_pos, new_line) 
endfunction
"}}}
" WrapEnvironment "{{{
" a:1 = 0 (or not present) called by a command
" a:1 = 1 called by a key map (ask for env)
function! <SID>WrapEnvironment(env_name,...)
    let map = ( a:0 == 0 ? 0 : a:1 ) 
    if !map
	'<,'>WrapSelection '\begin{'.a:env_name.'}','\end{'.a:env_name.'}','0', '1'
    else
	let envs=sort(filter(EnvCompletion("","",""), "v:val !~ '\*$' && v:val != 'thebibliography'"))
	let g:envs=envs
	" adjust the list - it is too long.
	let envs_a=copy(envs)
	call map(envs_a, "index(envs_a, v:val)+1.'. '.v:val")
	for line in atplib#PrintTable(envs_a,3)
	    echo line
	endfor
	let envs_a=['Which environment to use:']+envs_a
	let env=input("Which environment to use? type number and press <enter> or type environemnt name, <tab> to complete, <none> for exit:\n","","customlist,EnvCompletion")
	let g:env=env
	if env == ""
	    return
	elseif env =~ '^\d\+$'
	    let env_name=get(envs, env-1, '')
	    if env_name == ''
		return
	    endif
	else
	    let env_name=env
	endif
	call <SID>WrapSelection('\begin{'.env_name.'}','\end{'.env_name.'}','0', '1')
    endif
endfunction "}}}
vmap <buffer> <silent> <Plug>WrapEnvironment		:<C-U>call <SID>WrapEnvironment('', 1)<CR>

" Inteligent Aling
" TexAlign {{{
" This needs Aling vim plugin.
function! TexAlign()
    let save_pos = getpos(".")
    let synstack = map(synstack(line("."), col(".")), 'synIDattr( v:val, "name")')

    let balign=searchpair('\\begin\s*{\s*array\s*}', '', '\\end\s*{\s*array\s*}', 'bnW')
    let [bmatrix, bmatrix_col]=searchpairpos('\\matrix\s*\[[^]]*\]\s*\zs{', '', '}', 'bnW', '', max([1, (line(".")-g:atp_completion_limits[2])]))
    if bmatrix
	let bpat = '\\matrix\s*\[[^]]*\]\s*{'
	let bline = bmatrix+1 
	let epat = '}'
	let AlignCtr = 'jl+ &'
	let env = "matrix"
    elseif balign
	let bpat = '\\begin\s*{\s*array\s*}'
	let bline = balign+1
	let epat = '\\end\s*{\s*array\s*}'
	let AlignCtr = 'Il+ &'
	let env = "array"
    elseif count(synstack, 'texMathZoneA') || count(synstack, 'texMathZoneAS')
	let bpat = '\\begin\s*{\s*align\*\=\s*}' 
	let epat = '\\end\s*{\s*align\*\=\s*}' 
	let AlignCtr = 'Il+ &'
	let env = "align"
    elseif count(synstack, 'texMathZoneB') || count(synstack, 'texMathZoneBS')
	let bpat = '\\begin\s*{\s*alignat\*\=\s*}' 
	let epat = '\\end\s*{\s*alignat\*\=\s*}' 
	let AlignCtr = 'Il+ &'
	let env = "alignat"
    elseif count(synstack, 'texMathZoneD') || count(synstack, 'texMathZoneDS')
	let bpat = '\\begin\s*{\s*eqnarray\*\=\s*}' 
	let epat = '\\end\s*{\s*eqnarray\*\=\s*}' 
	let AlignCtr = 'Il+ &'
	let env = "eqnarray"
    elseif count(synstack, 'texMathZoneE') || count(synstack, 'texMathZoneES')
	let bpat = '\\begin\s*{\s*equation\*\=\s*}' 
	let epat = '\\end\s*{\s*equation\*\=\s*}' 
	let AlignCtr = 'Il+ =+-'
	let env = "equation"
    elseif count(synstack, 'texMathZoneF') || count(synstack, 'texMathZoneFS')
	let bpat = '\\begin\s*{\s*flalign\*\=\s*}' 
	let epat = '\\end\s*{\s*flalign\*\=\s*}' 
	let AlignCtr = 'jl+ &'
	let env = "falign"
"     elseif count(synstack, 'texMathZoneG') || count(synstack, 'texMathZoneGS')
"     gather doesn't need alignment (by design it give unaligned equation.
" 	let bpat = '\\begin\s*{\s*gather\*\=\s*}' 
" 	let epat = '\\end\s*{\s*gather\*\=\s*}' 
" 	let AlignCtr = 'Il+ &'
" 	let env = "gather"
    elseif count(synstack, 'displaymath')
	let bpat = '\\begin\s*{\s*displaymath\*\=\s*}' 
	let epat = '\\end\s*{\s*displaymath\*\=\s*}' 
	let AlignCtr = 'Il+ =+-'
	let env = "displaymath"
    elseif searchpair('\\begin\s*{\s*tabular\s*\}', '', '\\end\s*{\s*tabular\s*}', 'bnW', '', max([1, (line(".")-g:atp_completion_limits[2])]))
	let bpat = '\\begin\s*{\s*tabular\*\=\s*}' 
	let epat = '\\end\s*{\s*tabular\*\=\s*}' 
	let AlignCtr = 'jl+ &'
	let env = "tabular"
    else
	return
    endif
	
"     let g:env=env

    if !exists("bline")
	let bline = search(bpat, 'cnb') + 1
    endif
    if env != "matrix"
	let eline = searchpair(bpat, '', epat, 'cn')  - 1
    else
	let saved_pos = getpos(".")
	call cursor(bmatrix, bmatrix_col)
	let eline = searchpair('{', '', '}', 'n')  - 1
	call cursor(saved_pos[1], saved_pos[2])
    endif

" 	let g:bline = bline
" 	let g:eline = eline

    if bline <= eline
	execute bline . ',' . eline . 'Align ' . AlignCtr
    endif

    call setpos(".", save_pos) 
endfunction
"}}}

" Insert() function, which is used to insert text depending on mode: text/math. 
" {{{ Insert()
" Should be called via an imap:
" imap <lhs> 	<Esc>:call Insert(text, math)<CR>a
" a:text	= text to insert in text mode
" a:math	= text to insert in math mode	
" a:1		= how much to move cursor to the left after inserti
function! Insert(text, math, ...)

    let move = ( a:0 >= 1 ? a:1 : 0 )

    if b:atp_TexFlavor == 'plaintex' && index(g:atp_MathZones, 'texMathZoneY') == -1
	call add(g:atp_MathZones, 'texMathZoneY')
    endif
    let MathZones = copy(g:atp_MathZones)

    " select the correct wrapper
    if atplib#CheckSyntaxGroups(MathZones, line("."), col("."))
	let insert	= a:math
    else
	let insert	= a:text
    endif

    " if the insert variable is empty return
    if empty(insert)
	return
    endif

    let line		= getline(".")
    let col		= col(".")

    let new_line	= strpart(line, 0, col) . insert . strpart(line, col)
    call setline(line("."), new_line)
    call cursor(line("."), col(".")+len(insert)-move)

    return ""
endfunction
" }}}
" Insert \item update the number. 
" {{{ InsertItem()
" ToDo: indent
function! InsertItem()
    let begin_line	= searchpair( '\\begin\s*{\s*\%(enumerate\|itemize\|thebibliography\)\s*}', '', '\\end\s*{\s*\%(enumerate\|itemize\|thebibliography\)\s*}', 'bnW')
    let saved_pos	= getpos(".")
    call cursor(line("."), 1)

    if g:atp_debugInsertItem
	let g:debugInsertItem_redir= "redir! > ".g:atp_TempDir."/InsertItem.log"
	exe "redir! > ".g:atp_TempDir."/InsertItem.log"
    endif

    if getline(begin_line) =~ '\\begin\s*{\s*thebibliography\s*}'
	call cursor(saved_pos[1], saved_pos[2])
	let new_line	= strpart(getline("."), 0, col(".")) . '\bibitem' . strpart(getline("."), col("."))
	call setline(line("."), new_line)

	" Indent the line:
	if &l:indentexpr != ""
	    let v:lnum=saved_pos[1]
	    execute "let indent = " . &l:indentexpr
	    let i 	= 1
	    let ind 	= ""
	    while i <= indent
		let ind	.= " "
		let i	+= 1
	    endwhile
	else
	    indent	= -1
	    ind 	=  matchstr(getline("."), '^\s*')
	endif
	call setline(line("."), ind . substitute(getline("."), '^\s*', '', ''))
	let saved_pos[2]	+= len('\bibitem') + indent
	call cursor(saved_pos[1], saved_pos[2])

	if g:atp_debugInsertItem
	    let g:InsertIntem_return = 0
	    silent echo "0] return"
	    redir END
	endif
	return
    endif

    " This will work with \item [[1]], but not with \item [1]]
    let [ bline, bcol]	= searchpos('\\item\s*\zs\[', 'b', begin_line) 
    if bline == 0
	call cursor(saved_pos[1], saved_pos[2])
	if search('\\item\>', 'nb', begin_line)
	    let new_line	= strpart(getline("."), 0, col(".")) . '\item '. strpart(getline("."), col("."))
	else
	    let new_line	= strpart(getline("."), 0, col(".")) . '\item'. strpart(getline("."), col("."))
	endif
	call setline(line("."), new_line)

	" Indent the line:
	if &l:indentexpr != ""
	    let v:lnum=saved_pos[1]
	    execute "let indent = " . &l:indentexpr
	    let i 	= 1
	    let ind 	= ""
	    while i <= indent
		let ind	.= " "
		let i	+= 1
	    endwhile
	else
	    indent	= -1
	    ind 	=  matchstr(getline("."), '^\s*')
	endif
	if g:atp_debugInsertItem
	    silent echo "1] indent=".len(ind)
	endif
	call setline(line("."), ind . substitute(getline("."), '^\s*', '', ''))

	" Set the cursor position
	let saved_pos[2]	+= len('\item') + indent
	keepjumps call setpos(".", saved_pos)

	if g:atp_debugInsertItem
	    let g:debugInsertItem_return = 1
	    silent echo "1] return"
	    redir END
	endif
	return ""
    endif
    let [ eline, ecol]	= searchpairpos('\[', '', '\]', 'nr', '', line("."))
    if eline != bline
	if g:atp_debugInsertItem
	    let g:debugInsertItem_return = 2
	    silent echo "2] return"
	    redir END
	endif
	return ""
    endif

    let item		= strpart(getline("."), bcol, ecol - bcol - 1)
    let bpat		= '(\|{\|\['
    let epat		= ')\|}\|\]\|\.'
    let number		= matchstr(item, '\d\+')
    let subNr		= matchstr(item, '\d\+\zs\a\ze')
    let space		= matchstr(getline("."), '\\item\zs\s*\ze\[')
    if nr2char(number) != "" && subNr == "" 
	let new_item	= substitute(item, number, number + 1, '')
	if g:atp_debugInsertItem
	    silent echo "(1) new_item=".new_item
	endif
    elseif item =~ '\%('.bpat.'\)\=\s*\%(i\|ii\|iii\|iv\|v\|vi\|vii\|viii\|ix\)\%('.epat.'\)\=$'
	let numbers	= [ 'i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x' ]
	let roman	= matchstr(item, '\%('.bpat.'\)\=\s*\zs\w\+\ze\s*\%('.epat.'\)\=$')
	let new_roman	= get(numbers, index(numbers, roman) + 1, 'xi') 
	let new_item	= substitute(item,  '^\%('.bpat.'\)\=\s*\zs\a\+\ze\s*\%('.epat.'\)\=$', new_roman, 'g') 
	if g:atp_debugInsertItem
	    silent echo "(2) new_item=".new_item
	endif
    elseif nr2char(number) != "" && subNr != ""
	let alphabet 	= [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'w', 'x', 'y', 'z' ] 
	let char	= matchstr(item, '^\%('.bpat.'\)\=\s*\d\+\zs\a\ze\s*\%('.epat.'\)\=$')
	let new_char	= get(alphabet, index(alphabet, char) + 1, 'z')
	let new_item	= substitute(item, '^\%('.bpat.'\)\=\s*\d\+\zs\a\ze\s*\%('.epat.'\)\=$', new_char, 'g')
	if g:atp_debugInsertItem
	    silent echo "(3) new_item=".new_item
	endif
    elseif item =~ '\%('.bpat.'\)\=\s*\w\s*\%('.epat.'\)\='
	let alphabet 	= [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'w', 'x', 'y', 'z' ] 
	let char	= matchstr(item, '^\%('.bpat.'\)\=\s*\zs\w\ze\s*\%('.epat.'\)\=$')
	let new_char	= get(alphabet, index(alphabet, char) + 1, 'a')
	let new_item	= substitute(item, '^\%('.bpat.'\)\=\s*\zs\w\ze\s*\%('.epat.'\)\=$', new_char, 'g')
	if g:atp_debugInsertItem
	    silent echo "(4) new_item=".new_item
	endif
    else
	let new_item	= item
	if g:atp_debugInsertItem
	    silent echo "(5) new_item=".item
	endif
    endif

    keepjumps call setpos(".", saved_pos)

    let new_line	= strpart(getline("."), 0, col(".")) . '\item' . space . '[' . new_item . '] ' . strpart(getline("."), col("."))
    if g:atp_debugInsertItem
	silent echo "new_line=".new_line
    endif
    call setline(line("."), new_line)

    " Indent the line:
    if &l:indentexpr != ""
	let v:lnum=saved_pos[1]
	execute "let indent = " . &l:indentexpr
	let i 	= 1
	let ind 	= ""
	while i <= indent
	    let ind	.= " "
	    let i	+= 1
	endwhile
    else
	ind 	= matchstr(getline("."), '^\s*')
    endif
    if g:atp_debugInsertItem
	silent echo "indent=".len(ind)
    endif
    call setline(line("."), ind . substitute(getline("."), '^\s*', '', ''))

    " Set the cursor position
    let saved_pos[2]	+= len('\item' . space . '[' . new_item . ']') + indent
    keepjumps call setpos(".", saved_pos)


    if g:atp_debugInsertItem
	let g:debugInsertItem_return = 3
	silent echo "3] return"
	redir END
    endif
    return ""
endfunction
" }}}

" Editing Toggle Functions
"{{{ Variables
if !exists("g:atp_no_toggle_environments")
    let g:atp_no_toggle_environments=[ 'document', 'tikzpicture', 'picture']
endif
if !exists("g:atp_toggle_environment_1")
    let g:atp_toggle_environment_1=[ 'center', 'flushleft', 'flushright', 'minipage' ]
endif
if !exists("g:atp_toggle_environment_2")
    let g:atp_toggle_environment_2=[ 'enumerate', 'itemize', 'list', 'description' ]
endif
if !exists("g:atp_toggle_environment_3")
    let g:atp_toggle_environment_3=[ 'quotation', 'quote', 'verse' ]
endif
if !exists("g:atp_toggle_environment_4")
    let g:atp_toggle_environment_4=[ 'theorem', 'proposition', 'lemma' ]
endif
if !exists("g:atp_toggle_environment_5")
    let g:atp_toggle_environment_5=[ 'corollary', 'remark', 'note' ]
endif
if !exists("g:atp_toggle_environment_6")
    let g:atp_toggle_environment_6=[  'equation', 'align', 'array', 'alignat', 'gather', 'flalign', 'multline'  ]
endif
if !exists("g:atp_toggle_environment_7")
    let g:atp_toggle_environment_7=[ 'smallmatrix', 'pmatrix', 'bmatrix', 'Bmatrix', 'vmatrix' ]
endif
if !exists("g:atp_toggle_environment_8")
    let g:atp_toggle_environment_8=[ 'tabbing', 'tabular']
endif
if !exists("g:atp_toggle_labels")
    let g:atp_toggle_labels=1
endif
"}}}
"{{{ ToggleStar
" this function adds a star to the current environment
" todo: to doc.
function! s:ToggleStar()

    " limit:
    let from_line=max([1,line(".")-g:atp_completion_limits[2]])
    let to_line=line(".")+g:atp_completion_limits[2]

    " omit pattern
    let no_star=copy(g:atp_no_star_environments)
    let cond = atplib#SearchPackage('mdwlist')
    if cond || exists("b:atp_LocalEnvironments") && index(b:atp_LocalEnvironments, 'enumerate*') != -1
	call remove(no_star, index(no_star, 'enumerate'))
    endif
    if cond || exists("b:atp_LocalEnvironments") && index(b:atp_LocalEnvironments, 'itemize') != -1
	call remove(no_star, index(no_star, 'itemize'))
    endif
    if cond || exists("b:atp_LocalEnvironments") && index(b:atp_LocalEnvironments, 'description') != -1
	call remove(no_star, index(no_star, 'description'))
    endif
    let omit=join(no_star,'\|')
    let open_pos=searchpairpos('\\begin\s*{','','\\end\s*{[^}]*}\zs','cbnW','getline(".") =~ "\\\\begin\\s*{".omit."}"',from_line)
    let env_name=matchstr(strpart(getline(open_pos[0]),open_pos[1]),'begin\s*{\zs[^}]*\ze}')
    if ( open_pos == [0, 0] || index(no_star, env_name) != -1 ) && getline(line(".")) !~ '\\\%(part\|chapter\|\%(sub\)\{0,2}section\)'
	return
    endif
    if env_name =~ '\*$'
	let env_name=substitute(env_name,'\*$','','')
	let close_pos=searchpairpos('\\begin\s*{'.env_name.'\*}','','\\end\s*{'.env_name.'\*}\zs','cnW',"",to_line)
	if close_pos != [0, 0]
	    call setline(open_pos[0],substitute(getline(open_pos[0]),'\(\\begin\s*{\)'.env_name.'\*}','\1'.env_name.'}',''))
	    call setline(close_pos[0],substitute(getline(close_pos[0]),
			\ '\(\\end\s*{\)'.env_name.'\*}','\1'.env_name.'}',''))
	    echomsg "[ATP:] star removed from '".env_name."*' at lines: " .open_pos[0]." and ".close_pos[0]
	endif
    else
	let close_pos=searchpairpos('\\begin\s{'.env_name.'}','','\\end\s*{'.env_name.'}\zs','cnW',"",to_line)
	if close_pos != [0, 0]
	    call setline(open_pos[0],substitute(getline(open_pos[0]),
		    \ '\(\\begin\s*{\)'.env_name.'}','\1'.env_name.'\*}',''))
	    call setline(close_pos[0],substitute(getline(close_pos[0]),
			\ '\(\\end\s*{\)'.env_name.'}','\1'.env_name.'\*}',''))
	    echomsg "[ATP:] star added to '".env_name."' at lines: " .open_pos[0]." and ".close_pos[0]
	endif
    endif

    " Toggle the * in \section, \chapter, \part commands.
    if getline(line(".")) =~ '\\\%(part\|chapter\|\%(sub\)\{0,2}section\)\*'
	let pos = getpos(".")
	substitute/\(\\part\|\\chapter\|\\\%(sub\)\{0,2}section\)\*/\1/
	call cursor(pos[1], pos[2])
    elseif getline(line(".")) =~ '\\\%(part\|chapter\|\%(sub\)\{0,2}section\)'
	let pos = getpos(".")
	substitute/\(\\part\|\\chapter\|\\\%(sub\)\{0,2}section\)/\1*/
	call cursor(pos[1], pos[2])
    endif
endfunction
"}}}
"{{{ ToggleEnvironment
" this function toggles envrionment name.
" Todo: to doc.
" a:ask = 0 toggle, 1 ask for the new env name if not given as the first argument. 
" the argument specifies the speed (if -1 then toggle back)
" default is '1' or the new environment name
try
function! s:ToggleEnvironment(ask, ...)

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    " l:add might be a number or an environment name
    " if it is a number the function will jump this amount in appropriate list
    " (g:atp_toggle_environment_[123...]) to find new environment name
    let l:add = ( a:0 >= 1 ? a:1 : 1 ) 

    " limit:
    let l:from_line=max([1,line(".")-g:atp_completion_limits[2]])
    let l:to_line=line(".")+g:atp_completion_limits[2]

    " omit pattern
    let l:omit=join(g:atp_no_toggle_environments,'\|')
    let l:open_pos=searchpairpos('\\begin\s*{','','\\end\s*{[^}]*}\zs','bnW','getline(".") =~ "\\\\begin\\s*{".l:omit."}"',l:from_line)
    let l:env_name=matchstr(strpart(getline(l:open_pos[0]),l:open_pos[1]),'begin\s*{\zs[^}]*\ze}')

    let l:label=matchstr(strpart(getline(l:open_pos[0]),l:open_pos[1]),'\\label\s*{\zs[^}]*\ze}')
    " DEBUG
"     let b:line=strpart(getline(l:open_pos[0]),l:open_pos[1])
"     let b:label=l:label
"     let b:env_name=l:env_name
    if l:open_pos == [0, 0] || index(g:atp_no_toggle_environments,l:env_name) != -1
	return
    endif

    let l:env_name_ws=substitute(l:env_name,'\*$','','')

    if !a:ask
	let l:variable="g:atp_toggle_environment_1"
	let l:i=1
	while 1
	    let l:env_idx=index({l:variable},l:env_name_ws)
	    if l:env_idx != -1
		break
	    else
		let l:i+=1
		let l:variable="g:atp_toggle_environment_".l:i
	    endif
	    if !exists(l:variable)
		return
	    endif
	endwhile

	if l:add > 0 && l:env_idx > len({l:variable})-l:add-1
	    let l:env_idx=0
	elseif ( l:add < 0 && l:env_idx < -1*l:add )
	    let l:env_idx=len({l:variable})-1
	else
	    let l:env_idx+=l:add
	endif
	let l:new_env_name={l:variable}[l:env_idx]
	if l:env_name =~ '\*$'
	    let l:new_env_name.="*"
	endif
    else
	if l:add == 1
	    let l:new_env_name=input("What is the new name for " . l:env_name . "? type and hit <Enter> ", "", "customlist,EnvCompletion" )
	    if l:new_env_name == ""
		redraw
		echomsg "[ATP:] environment name not changed"
		return
	    endif
	else
	    let l:new_env_name = l:add
	endif
    endif

    " DEBUG
"     let g:i=l:i
"     let g:env_idx=l:env_idx
"     let g:env_name=l:env_name
"     let g:add = l:add
"     let g:new_env_name=l:new_env_name

    let l:env_name=escape(l:env_name,'*')
    let l:close_pos=searchpairpos('\\begin\s*{'.l:env_name.'}','','\\end\s*{'.l:env_name.'}\zs','nW',"",l:to_line)
    if l:close_pos != [0, 0]
	call setline(l:open_pos[0],substitute(getline(l:open_pos[0]),'\(\\begin\s*{\)'.l:env_name.'}','\1'.l:new_env_name.'}',''))
	call setline(l:close_pos[0],substitute(getline(l:close_pos[0]),
		    \ '\(\\end\s*{\)'.l:env_name.'}','\1'.l:new_env_name.'}',''))
	redraw
	echomsg "[ATP:] environment toggeled at lines: " .l:open_pos[0]." and ".l:close_pos[0]
    endif

    if l:label != "" && g:atp_toggle_labels
	if l:env_name == ""
	    let l:new_env_name_ws=substitute(l:new_env_name,'\*$','','')
	    let l:new_short_name=get(g:atp_shortname_dict,l:new_env_name_ws,"")
	    let l:new_label =  l:new_short_name . strpart(l:label, stridx(l:label, g:atp_separator))
" 	    let g:new_label = l:new_label . "XXX"
	else
" 	    let g:label = l:label
	    let l:new_env_name_ws=substitute(l:new_env_name,'\*$','','')
" 	    let g:new_env_name_ws=l:new_env_name_ws
	    let l:new_short_name=get(g:atp_shortname_dict,l:new_env_name_ws,"")
" 	    let g:new_short_name=l:new_short_name
	    let l:short_pattern= '^\(\ze:\|' . join(values(filter(g:atp_shortname_dict,'v:val != ""')),'\|') . '\)'
" 	    let g:short_pattern=l:short_pattern
	    let l:short_name=matchstr(l:label, l:short_pattern)
" 	    let g:short_name=l:short_name
	    let l:new_label=substitute(l:label,'^'.l:short_name,l:new_short_name,'')
" 	    let g:new_label=l:new_label
	endif


	" check if new label is in use!
	let pos_save=getpos(".")
	let n=search('\m\C\\\(label\|\%(eq\|page\)\?ref\)\s*{'.l:new_label.'}','nwc')

	if n == 0 && l:new_label != l:label
	    let hidden =  &hidden
	    set hidden
	    silent! keepjumps execute l:open_pos[0].'substitute /\\label{'.l:label.'}/\\label{'.l:new_label.'}'
	    " This should be done for every file in the project. 
	    if !exists("b:TypeDict")
		call TreeOfFiles(atp_MainFile)
	    endif
	    let save_view 	= winsaveview()
	    let file		= expand("%:p")
	    let project_files = keys(filter(b:TypeDict, "v:val == 'input'")) + [ atp_MainFile ]
	    for project_file in project_files
		if atplib#FullPath(project_file) != expand("%:p")
		    exe "silent keepalt edit " . project_file
		endif
		let pos_save_pf=getpos(".")
		silent! keepjumps execute '%substitute /\\\(eq\|page\)\?\(ref\s*\){'.l:label.'}/\\\1\2{'.l:new_label.'}/gIe'
		keepjumps call setpos(".", pos_save_pf)
	    endfor
	    execute "keepalt buffer " . file
	    keepjumps call setpos(".", pos_save)
	    let &hidden = hidden
	elseif n != 0 && l:new_label != l:label
	    redraw
	    echohl WarningMsg
	    echomsg "[ATP:] labels not changed, new label: ".l:new_label." is in use!"
	    echohl Normal
	endif
    endif
    return  l:open_pos[0]."-".l:close_pos[0]
endfunction
catch /E127:/
endtry "}}}

" This is completion for input() inside ToggleEnvironment which uses
" b:atp_LocalEnvironments variable.
function! EnvCompletion(ArgLead, CmdLine, CursorPos) "{{{
    if !exists("b:atp_LocalEnvironments")
	LocalCommands
    endif

    let env_list = copy(b:atp_LocalEnvironments)
    " add standard and ams environment if not present.
    let env_list=atplib#Extend(env_list, g:atp_Environments)
    if atplib#SearchPackage('amsmath')
	let env_list=atplib#Extend(env_list, g:atp_amsmath_environments)
    endif
    call filter(env_list, "v:val =~# '^' .a:ArgLead")
    return env_list
endfunction "}}}
function! EnvCompletionWithoutStarEnvs(ArgLead, CmdLine, CursorPos) "{{{
    if !exists("b:atp_LocalEnvironments")
	LocalCommands
    endif

    let env_list = copy(b:atp_LocalEnvironments)
    " add standard and ams environment if not present.
    let env_list=atplib#Extend(env_list, g:atp_Environments)
    if atplib#SearchPackage('amsmath')
	let env_list=atplib#Extend(env_list, g:atp_amsmath_environments)
    endif
    call filter(env_list, "v:val =~# '^' .a:ArgLead")
    return env_list
endfunction "}}}
function! F_compl(ArgLead, CmdLine, CursorPos) "{{{
    " This is like EnvCompletion but without stared environments and with: chapter, section, ...
    if !exists("b:atp_LocalEnvironments")
	LocalCommands
    endif

    let env_list = copy(b:atp_LocalEnvironments)
    " add standard and ams environment if not present.
    let env_list=atplib#Extend(env_list, g:atp_Environments)
    let env_list=atplib#Extend(env_list, ['part', 'chapter', 'section', 'subsection', 'subsubsection'])
    if atplib#SearchPackage('amsmath')
	let env_list=atplib#Extend(env_list, g:atp_amsmath_environments)
    endif
    call filter(env_list+['math'], "v:val !~ '\*$'")
    return join(env_list, "\n")
endfunction "}}}
" TexDoc commanand and its completion
" {{{ TexDoc 
" This is non interactive !, use :!texdoc for interactive command.
" But it simulates it with a nice command completion (Ctrl-D, <Tab>)
" based on alias files for texdoc.
function! s:TexDoc(...)
    let texdoc_arg	= ""
    for i in range(1,a:0)
	let texdoc_arg.=" " . a:{i}
    endfor
    if texdoc_arg == ""
	let texdoc_arg 	= g:atp_TeXdocDefault
    endif
    " If the file is a text file texdoc is 'cat'-ing it into the terminal,
    " we use echo to capture the output. 
    " The rediraction prevents showing texdoc info messages which are not that
    " important, if a document is not found texdoc sends a message to the standard
    " output not the error.
    "
    " -I prevents from using interactive menus
    echo system("texdoc " . texdoc_arg . " 2>/dev/null")
endfunction

function! s:TeXdoc_complete(ArgLead, CmdLine, CursorPos)
    let texdoc_alias_files=split(system("texdoc -f"), '\n')
    call filter(texdoc_alias_files, "v:val =~ 'active'")
    call map(texdoc_alias_files, "substitute(substitute(v:val, '^[^/]*\\ze', '', ''), '\/\/\\+', '/', 'g')")
    let aliases = []
    for file in texdoc_alias_files
	call extend(aliases, readfile(file))
    endfor

    call filter(aliases, "v:val =~ 'alias'")
    call filter(map(aliases, "matchstr(v:val, '^\\s*alias\\s*\\zs\\S*\\ze\\s*=')"),"v:val !~ '^\\s*$'")
    if exists("g:atp_LatexPackages")
	call extend(aliases, g:atp_LatexPackages)
    endif

    return filter(copy(aliases), "v:val =~ '^' . a:ArgLead")
endfunction
" }}}

" This function deletes tex specific output files (exept the pdf/dvi file, unless
" bang is used - then also delets the current output file)
" {{{ Delete
function! <SID>Delete(delete_output)

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    call atplib#outdir()

    let atp_tex_extensions=deepcopy(g:atp_tex_extensions)

    if a:delete_output == "!" || g:atp_delete_output == 1
	let ext = substitute(get(g:atp_CompilersDict,b:atp_TexCompiler,""), '^\s*\.', '', 'g')
	if ext != ""
	    call add(atp_tex_extensions,ext)
	endif
    else
	" filter extensions which should not be deleted
	call filter(atp_tex_extensions, "index(g:atp_DeleteWithBang, v:val) == -1")
    endif

    " Be sure that we are not deleting outputs:
    for ext in atp_tex_extensions
	if ext != "pdf" && ext != "dvi" && ext != "ps"
	    let files=split(globpath(fnamemodify(atp_MainFile, ":h"), "*.".ext), "\n")
	    if files != []
		echo "Removing *.".ext
		for f in files
		    call delete(f)
		endfor
	    endif
	else
	    " Delete output file (pdf|dvi|ps) (though ps is not supported by ATP).
	    let f=fnamemodify(atplib#FullPath(b:atp_MainFile), ":r").".".ext
	    echo "Removing ".f
	    call delete(f)
	endif
    endfor
endfunction
"}}}

"{{{ OpenLog, TexLog, TexLog Buffer Options, PdfFonts, YesNoCompletion
"{{{ s:Search function for Log Buffer
function! <SID>Search(pattern, flag, ...)
    echo ""
    let center 	= ( a:0 >= 1 ? a:1 : 1 )
    let @/	= a:pattern

    " Count:
"     let nr 	= 1
"     while nr <= a:count
" 	let keepjumps = ( a:nr < a:count ? 'keepjumps' : '')
" 	exe keepjumps . "let line = search(a:pattern, a:flag)"
" 	let nr	+= 1
"     endwhile

    let line = search(a:pattern, a:flag)

    if !line
	let message = a:flag =~# 'b' ? 'previous' : 'next'
	if a:pattern =~ 'warning'
	    let type = 'warning'
	elseif a:pattern =~ '!$'
	    let type = 'error'
	elseif a:pattern =~ 'info'
	    let type = 'info'
	else
	    let type = ''
	endif
	echohl WarningMsg
	echo "No " . message . " " . type . " message."
	echohl Normal
    endif
" This fails (?):
"     if center
" 	normal zz
"     endif
endfunction
" command! -count=1 -nargs=* <SID>Search	:call <SID>Search(<count>,<args>)

function! <SID>Searchpair(start, middle, end, flag, ...)
    let center 	= ( a:0 >= 1 ? a:1 : 1 )
    if getline(".")[col(".")-1] == ')' 
	let flag= a:flag.'b'
    else
	let flag= substitute(a:flag, 'b', '', 'g')
    endif
    call searchpair(a:start, a:middle, a:end, flag)
"     if center
" 	normal zz
"     endif
endfunction
"}}}
function! s:OpenLog()
    if filereadable(&l:errorfile)

	let projectVarDict = SaveProjectVariables()
	let g:projectVarDict = projectVarDict
	let s:winnr	= bufwinnr("")
	let atp_TempDir	= b:atp_TempDir
	exe "rightbelow split +setl\\ nospell\\ ruler\\ syn=log_atp\\ autoread " . fnameescape(&l:errorfile)
	let b:atp_TempDir = atp_TempDir
	call RestoreProjectVariables(projectVarDict)

	map <buffer> q :bd!<CR>
	nnoremap <silent> <buffer> ]m :call <SID>Search('\CWarning\\|^!', 'W')<CR>
	nnoremap <silent> <buffer> [m :call <SID>Search('\CWarning\\|^!', 'bW')<CR>
	nnoremap <silent> <buffer> ]w :call <SID>Search('\CWarning', 'W')<CR>
	nnoremap <silent> <buffer> [w :call <SID>Search('\CWarning', 'bW')<CR>
	nnoremap <silent> <buffer> ]c :call <SID>Search('\CLaTeX Warning: Citation', 'W')<CR>
	nnoremap <silent> <buffer> [c :call <SID>Search('\CLaTeX Warning: Citation', 'bW')<CR>
	nnoremap <silent> <buffer> ]r :call <SID>Search('\CLaTeX Warning: Reference', 'W')<CR>
	nnoremap <silent> <buffer> [r :call <SID>Search('\CLaTeX Warning: Reference', 'bW')<CR>
	nnoremap <silent> <buffer> ]e :call <SID>Search('^[^!].*\n\zs!', 'W')<CR>
	nnoremap <silent> <buffer> [e :call <SID>Search('^[^!].*\n\zs!', 'bW')<CR>
	nnoremap <silent> <buffer> ]f :call <SID>Search('\CFont \%(Info\\|Warning\)', 'W')<CR>
	nnoremap <silent> <buffer> [f :call <SID>Search('\CFont \%(Info\\|Warning\)', 'bW')<CR>
	nnoremap <silent> <buffer> ]p :call <SID>Search('\CPackage', 'W')<CR>
	nnoremap <silent> <buffer> [p :call <SID>Search('\CPackage', 'bW')<CR>
	nnoremap <silent> <buffer> ]P :call <SID>Search('\[\_d\+\zs', 'W')<CR>
	nnoremap <silent> <buffer> [P :call <SID>Search('\[\_d\+\zs', 'bW')<CR>
	nnoremap <silent> <buffer> ]i :call <SID>Search('\CInfo', 'W')<CR>
	nnoremap <silent> <buffer> [i :call <SID>Search('\CInfo', 'bW')<CR>
	nnoremap <silent> <buffer> % :call <SID>Searchpair('(', '', ')', 'W')<CR>

"	This prevents vim from reloading with 'autoread' option: the buffer is
"	modified outside and inside vim.
	try
	    silent! execute 'keepjumps %g/^\s*$/d'
	    silent! execute "keepjumps normal ''"
	catch /E486:/ 
	endtry
		   
	function! <SID>SyncTex(bang,...)

	    let cwd = getcwd()
	    exe "lcd " . fnameescape(b:atp_ProjectDir)

	    let g:debugST	= 0

	    " if sync = 1 sync log file and the window - can be used by autocommand
	    let sync = ( a:0 >= 1 ? a:1 : 0 )
		if g:atp_debugST
		    let g:sync = sync
		endif 

	    if sync && !g:atp_LogSync
		exe "normal! " . cwd
		let g:debugST 	= 1
		return
	    endif

	    " Find the end pos of error msg
	    keepjumps let [ stopline, stopcol ] = searchpairpos('(', '', ')', 'nW') 
		if g:atp_debugST
		    let g:stopline = stopline
		endif

	    let saved_pos = getpos(".")

	    " Be linewise
	    call setpos(".", [0, line("."), 1, 0])

	    " Find the line nr
" 	    keepjumps let [ LineNr, ColNr ] = searchpos('^l.\zs\d\+\>\|oninput line \zs\|at lines \zs', 'W', stopline)
	    keepjumps let [ LineNr, ColNr ] = searchpos('^l.\zs\d\+\>\|o\n\=n\_s\+i\n\=n\n\=p\n\=u\n\=t\_s\+l\n\=i\n\=n\n\=e\_s\+\zs\|a\n\=t\_s\+l\n\=i\n\=n\n\=e\n\=s\_s\+\zs', 'W', stopline)
	    let line	= strpart(getline(LineNr), ColNr-1)
	    let lineNr 	= matchstr(line, '^\d\+\ze')
		let g:lineNr=lineNr
	    if lineNr !~ '\d\+'
		keepjumps call setpos(".", saved_pos)
		return
	    endif
	    if getline(LineNr) =~ '^l\.\d\+'
		let error = escape(matchstr(getline(LineNr), '^l\.\d\+\s*\zs.*$'), '\.')
" 		let error = escape(matchstr(getline(LineNr), '^l\.\d\+\s*\zs.*$'), '\.') . '\s*' .  escape(substitute(strpart(getline(LineNr+1), 0, stridx(getline(LineNr+1), '...')), '^\s*', '', ''), '\.')
		if g:atp_debugST
		    let g:error = error
		endif
	    endif

	    " Find the file name/bufnr/winnr where the error occurs. 
	    let test 	= 0
	    let nr	= 0
	    " There should be a finer way to get the file name if it is split in two
	    " lines.
	    if g:atp_debugST
		let g:fname_list = []
	    endif
	    while !test
		" Some times in the lof file there is a '(' from the source tex file
		" which might be not closed, then this while loop is used to find
		" readable file name.
		let [ startline, startcol ] = searchpairpos('(', '', ')', 'bW') 
		if g:atp_debugST
		    exe "redir! > ".g:atp_TempDir."/SyncTex.log"
		    let g:startline = startline
		    silent! echomsg " [ startline, startcol ] " . string([ startline, startcol ])
		endif
" THIS CODE IS NOT WORKING:
" 		if nr >= 1 && [ startline, startcol ] == [ startline_o, startcol_o ] && !test
" 		    keepjumps call setpos(".", saved_pos)
" 		    break
" 		endif
		if !startline
		    if g:atp_debugST
			silent! echomsg "END startline = " . startline
			redir END
		    endif
		    keepjumps call setpos(".", saved_pos)
		    return
		endif
		let fname 	= matchstr(strpart(getline(startline), startcol), '^\f\+') 
		" if the file name was broken in the log file in two lines,
		" get the end of file name from the next line. 
		let tex_extensions = extend(copy(g:atp_tex_extensions), [ 'tex', 'cls', 'sty', 'clo', 'def' ], 0)
		let pat = '\.\%('.join(tex_extensions, '\|').'\)$'
		if fname !~# pat
		    let stridx = {}
		    for end in tex_extensions
			call extend(stridx, { end : stridx(getline(startline+1), "." . end) })
		    endfor
		    call filter(stridx, "v:val != -1")
		    let StrIdx = {}
		    for end in keys(stridx)
			call extend(StrIdx, { stridx[end] : end }, 'keep')
		    endfor
		    let idx = min(keys(StrIdx))
		    let end = get(StrIdx, idx, "")
		    let fname .= strpart(getline(startline+1), 0, idx + len(end) + 1)
		endif
		let fname=substitute(fname, escape(fnamemodify(b:atp_TempDir, ":t"), '.').'\/[^\/]*\/', '', '')
		if g:atp_debugST
		    call add(g:fname_list, fname)
		    let g:fname = fname
		    let g:dir	= fnamemodify(g:fname, ":p:h")
		    let g:pat	= pat
" 		    if g:fname =~# '^' .  escape(fnamemodify(tempname(), ":h"), '\/')
" 			let g:fname = substitute(g:fname, fnamemodify(tempname(), ":h"), b:atp_ProjectDir)
" 		    endif
		endif
		let test 	= filereadable(fname)
		let nr	+= 1
		let [ startline_o, startcol_o ] = deepcopy([ startline, startcol ])
	    endwhile
	    keepjumps call setpos(".", saved_pos)
		if g:atp_debugST
		    let g:fname_post = fname
		endif

	    " if the file is under texmf directory return unless g:atp_developer = 1
	    " i.e. do not visit packages and classes.
	    if ( fnamemodify(fname, ':p') =~ '\%(\/\|\\\)texmf' || index(['cls', 'sty', 'bst'], fnamemodify(fname, ":e")) != -1 ) && !g:atp_developer
		keepjumps call setpos(".", saved_pos)
		return
	    elseif fnamemodify(fname, ':p') =~ '\%(\/\|\\\)texmf'
		" comma separated list of options
	    	let options = 'nospell'
	    else
		let options = ''
	    endif

	    let bufnr = bufnr(fname)
" 		let g:bufnr = bufnr
	    let bufwinnr	= bufwinnr(bufnr)
	    let log_winnr	= bufwinnr("")

	    " Goto found file and correct line.
	    " with bang open file in a new window,
	    " without open file in previous window.
	    if a:bang == "!"
		if bufwinnr != -1
		    exe bufwinnr . " wincmd w"
		    exe ':'.lineNr
		    exe 'normal zz'
		elseif buflisted(bufnr)
		    exe 'split #' . bufnr
		    exe ':'.lineNr
		    exe 'normal zz'
		else
		    " allows to go to errrors in packages.
		    exe 'split ' . fname
		    exe ':'.lineNr
		    exe 'normal zz'
		endif
	    else
		if bufwinnr != -1
		    exe bufwinnr . " wincmd w"
		    exe ':'.lineNr
		    exe 'normal zz'
		else
		    exe s:winnr . " wincmd w"
		    if buflisted(bufnr)
			exe "b " . bufnr
			exe ':'.lineNr
			exe 'normal zz'
		    else
			exe "edit " . fname
			exe ':'.lineNr
			exe 'normal zz'
		    endif
		    exe 'normal zz'
		endif
	    endif

	    " set options
		if &filetype == ""
		    filetype detect
		endif
		for option in split(options, ',')
		    exe "setl " . option
		endfor

	    " highlight the error
	    if exists("error") && error != ""
" 		let error_pat = escape(error, '\.')
" 		call matchadd("ErrorMsg", '\%'.lineNr.'l' . error_pat) 
		let matchID =  matchadd("Error", error, 15) 
	    endif

	    if sync
		setl cursorline
		" Unset 'cursorline' option when entering the window. 
		exe 'au! WinEnter ' . expand("%:p")  . " setl nocursorline"
" 		if exists("matchID")
" 		    exe 'au! WinEnter ' . expand("%:p")  . " call matchdelete(".matchID.")"
" 		endif
		exe log_winnr . ' wincmd w'
	    else
		setl nocursorline
	    endif

	    exe "lcd " . fnameescape(cwd)
	endfunction
	command! -buffer -bang SyncTex		:call <SID>SyncTex(<q-bang>)
	map <buffer> <Enter>			:SyncTex<CR>
" 	nnoremap <buffer> <LocalLeader>g	:SyncTex<CR>	
	augroup ATP_SyncLog
	    au CursorMoved *.log :call <SID>SyncTex("", 1)
	augroup END

	function! s:SyncXpdfLog(...)

	    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
	    " check the value of g:atp_SyncXpdfLog
	    let check = ( a:0 >= 1 ? a:1 : 1 )

	    if b:atp_Viewer !~ '^\s*xpdf\>' || b:atp_XpdfServer == "" || check && !g:atp_SyncXpdfLog
		return
	    endif

" 	    let saved_pos	= getpos(".")	

	    let [ lineNr, colNr ] 	= searchpos('\[\_d\+\%({[^}]*}\)\=\n\=\]', 'n')
	    let line 	= strpart(getline(lineNr), colNr-1) . getline(lineNr+1)

	    let pageNr	= substitute(matchstr(line, '\[\zs\_d\+\ze\%({[^}]*}\)\=\]'), "\n", "", "g")
	    let g:pageNr	= pageNr

	    if pageNr	!= ""
		let cmd = "xpdf -remote " . b:atp_XpdfServer . " " . fnamemodify(atp_MainFile, ":r") . ".pdf " . pageNr . " &"
		let g:cmd = cmd
		call system(cmd)
	    endif
	endfunction
	command! -buffer SyncXpdf 	:call s:SyncXpdfLog(0)
	command! -buffer Xpdf 		:call s:SyncXpdfLog(0)
	map <buffer> <silent> <F3> 	:SyncXpdf<CR>
	augroup ATP_SyncXpdfLog
	    au CursorMoved *.log :call s:SyncXpdfLog(1)
	augroup END

    else
	echo "No log file"
    endif
endfunction

" TeX LOG FILE
if &buftype == 'quickfix'
	setlocal modifiable
	setlocal autoread
endif	
function! s:TexLog(options)
    if executable("texloganalyser")
       let s:command="texloganalyser " . a:options . " " . &l:errorfile
       echo system(s:command)
    else	
       echo "Please install 'texloganalyser' to have this functionality. The perl program written by Thomas van Oudenhove."  
    endif
endfunction

function! s:PdfFonts()
    if b:atp_OutDir !~ "\/$"
	b:atp_OutDir=b:atp_OutDir . "/"
    endif
    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    if executable("pdffonts")
	let s:command="pdffonts " . fnameescape(fnamemodify(atp_MainFile,":r")) . ".pdf"
	echo system(s:command)
    else
	echo "Please install 'pdffonts' to have this functionality. In 'gentoo' it is in the package 'app-text/poppler-utils'."  
    endif
endfunction	

" function! s:setprintexpr()
"     if b:atp_TexCompiler == "pdftex" || b:atp_TexCompiler == "pdflatex"
" 	let s:ext = ".pdf"
"     else
" 	let s:ext = ".dvi"	
"     endif
"     let &printexpr="system('lpr' . (&printdevice == '' ? '' : ' -P' . &printdevice) . ' " . fnameescape(fnamemodify(expand("%"),":p:r")) . s:ext . "') . + v:shell_error"
" endfunction
" call s:setprintexpr()

function! YesNoCompletion(A,P,L)
    return ['yes','no']
endfunction
"}}}

" Ssh printing tools
"{{{ Print, Lpstat, ListPrinters
" This function can send the output file to local or remote printer.
" a:1   = file to print		(if not given printing the output file)
" a:3	= printing options	(give printing optinos or 'default' then use
" 				the variable g:printingoptions)
 function! s:SshPrint(...)

    call atplib#outdir()

    " set the extension of the file to print
    " if prining the tex output file.
    if a:0 == 0 || a:0 >= 1 && a:1 == ""
	let ext = get(g:atp_CompilersDict, b:atp_TexCompiler, "not present")
	if ext == "not present"
	    echohl WarningMsg
	    echomsg "[ATP:] ".b:atp_TexCompiler . " is not present in g:atp_CompilersDict"
	    echohl Normal
	    return "extension not found"
	endif
	if b:atp_TexCompiler =~ "lua"
	    if b:atp_TexOptions == "" || b:atp_TexOptions =~ "output-format=\s*pdf"
		let ext = ".pdf"
	    else
		let ext = ".dvi"
	    endif
	endif
    endif

    " set the file to print
    let pfile		= ( a:0 == 0 || (a:0 >= 1 && a:1 == "" ) ? b:atp_OutDir . fnamemodify(expand("%"),":t:r") . ext : a:1 )

    " set the printing command
    let lprcommand	= g:atp_lprcommand
    if a:0 >= 2
	let arg_list	= copy(a:000)
	call remove(arg_list,0)
	let print_options	= join(arg_list, " ")
    endif

    " print locally or remotely
    " the default is to print locally (g:atp_ssh=`whoami`@localhost)
    let server	= ( exists("g:atp_ssh") ? strpart(g:atp_ssh,stridx(g:atp_ssh,"@")+1) : "localhost" )

    echomsg "[ATP:] server " . server
    echomsg "[ATP:] file   " . pfile

    if server =~ 'localhost'
	let cmd	= lprcommand . " " . print_options . " " .  fnameescape(pfile)

	redraw!
	echomsg "[ATP:] printing ...  " . cmd
	call system(cmd)
"     " print over ssh on the server g:atp_ssh with the printer a:1 (or the
    " default system printer if a:0 == 0
    else 
	let cmd="cat " . fnameescape(pfile) . " | ssh " . g:atp_ssh . " " . lprcommand . " " . print_options
	call system(cmd)
    endif
endfunction

function! <SID>Lpr(...)
    call atplib#outdir()

    " set the extension of the file to print
    " if prining the tex output file.
    if a:0 == 0 || a:0 >= 1 && a:1 == ""
	let ext = get(g:atp_CompilersDict, b:atp_TexCompiler, "not present")
	if ext == "not present"
	    echohl WarningMsg
	    echomsg "[ATP:] ".b:atp_TexCompiler . " is not present in g:atp_CompilersDict"
	    echohl Normal
	    return "extension not found"
	endif
	if b:atp_TexCompiler =~ "lua"
	    if b:atp_TexOptions == "" || b:atp_TexOptions =~ "output-format=\s*pdf"
		let ext = ".pdf"
	    else
		let ext = ".dvi"
	    endif
	endif
    endif

    " set the file to print
    let pfile		= ( a:0 == 0 || (a:0 >= 1 && a:1 == "" ) ? b:atp_OutDir . fnamemodify(expand("%"),":t:r") . ext : a:1 )
    
    " set the printing command
    let lprcommand	= g:atp_lprcommand
    if a:0 >= 1
	let arg_list	= copy(a:000)
	let print_options	= join(arg_list, " ")
    endif

    let cmd	= lprcommand . " " . print_options . " " .  fnameescape(pfile)

    redraw!
    echomsg "[ATP:] printing ...  " . cmd
    call system(cmd)
endfunction
" The command only prints the output file.
fun! s:Lpstat()
    if exists("g:apt_ssh") 
	let server=strpart(g:atp_ssh,stridx(g:atp_ssh,"@")+1)
    else
	let server='locahost'
    endif
    if server == 'localhost'
	echo system("lpstat -l")
    else
	echo system("ssh " . g:atp_ssh . " lpstat -l ")
    endif
endfunction

" This function is used for completetion of the command SshPrint
function! s:ListPrinters(A,L,P)
    if exists("g:atp_ssh") && g:atp_ssh !~ '@localhost' && g:atp_ssh != ""
	let cmd="ssh -q " . g:atp_ssh . " lpstat -a | awk '{print $1}'"
    else
	let cmd="lpstat -a | awk '{print $1}'"
    endif
    return system(cmd)
endfunction

function! s:ListLocalPrinters(A,L,P)
    let cmd="lpstat -a | awk '{print $1}'"
    return system(cmd)
endfunction

" custom style completion
function! s:Complete_lpr(ArgLead, CmdLine, CPos)
    if a:CmdLine =~ '-[Pd]\s\+\w*$'
	" complete printers
	return s:ListPrinters(a:ArgLead, "", "")
    elseif a:CmdLine =~ '-o\s\+[^=]*$'
	" complete option
	return join(g:atp_CupsOptions, "\n")
    elseif a:CmdLine =~ '-o\s\+Collate=\%(\w\|-\)*$'
	return join(['Collate=True', 'Collate=False'], "\n")
    elseif a:CmdLine =~ '-o\s\+page-set=\%(\w\|-\)*$'
	return join(['opage-set=odd', 'page-set=even'], "\n")
    elseif a:CmdLine =~ '-o\s\+sides=\%(\w\|-\)*$'
	return join(['sides=two-sided-long-edge', 'sides=two-sided-short-edge', 'sides=one-sided'], "\n")
    elseif a:CmdLine =~ '-o\s\+outputorder=\%(\w\|-\)*$'
	return join(['outputorder=reverse', 'outputorder=normal'], "\n")
    elseif a:CmdLine =~ '-o\s\+page-border=\%(\w\|-\)*$'
	return join(['page-border=double', 'page-border=none', 'page-border=double-thick', 'page-border=single', 'page-border=single-thick'], "\n")
    elseif a:CmdLine =~ '-o\s\+job-sheets=\%(\w\|-\)*$'
	return join(['job-sheets=none', 'job-sheets=classified', 'job-sheets=confidential', 'job-sheets=secret', 'job-sheets=standard', 'job-sheets=topsecret', 'job-sheets=unclassified'], "\n")
    elseif a:CmdLine =~ '-o\s\+number-up-layout=\%(\w\|-\)*$'
	return join(['number-up-layout=btlr', 'number-up-layout=btrl', 'number-up-layout=lrbt', 'number-up-layout=lrtb', 'number-up-layout=rlbt', 'number-up-layout=rltb', 'number-up-layout=tblr', 'number-up-layout=tbrl'], "\n")
    elseif a:CmdLine =~ '-o\s\+position=\%(\w\|-\)*$'
	return join(['position=center', 'position=top', 'position=left', 'position=right', 'position=top-left', 'position=top-right', 
		    \ 'position=bottom', 'position=bottom-left', 'position=bottom-right'], "\n")
    endif
    return ""
endfunction

function! s:CompleteLocal_lpr(ArgLead, CmdLine, CPos)
    if a:CmdLine =~ '-[Pd]\s\+\w*$'
	" complete printers
	return s:ListLocalPrinters(a:ArgLead, "", "")
    elseif a:CmdLine =~ '-o\s\+[^=]*$'
	" complete option
	return join(g:atp_CupsOptions, "\n")
    elseif a:CmdLine =~ '-o\s\+Collate=\%(\w\|-\)*$'
	return join(['Collate=True', 'Collate=False'], "\n")
    elseif a:CmdLine =~ '-o\s\+page-set=\%(\w\|-\)*$'
	return join(['opage-set=odd', 'page-set=even'], "\n")
    elseif a:CmdLine =~ '-o\s\+sides=\%(\w\|-\)*$'
	return join(['sides=two-sided-long-edge', 'sides=two-sided-short-edge', 'sides=one-sided'], "\n")
    elseif a:CmdLine =~ '-o\s\+outputorder=\%(\w\|-\)*$'
	return join(['outputorder=reverse', 'outputorder=normal'], "\n")
    elseif a:CmdLine =~ '-o\s\+page-border=\%(\w\|-\)*$'
	return join(['page-border=double', 'page-border=none', 'page-border=double-thick', 'page-border=single', 'page-border=single-thick'], "\n")
    elseif a:CmdLine =~ '-o\s\+job-sheets=\%(\w\|-\)*$'
	return join(['job-sheets=none', 'job-sheets=classified', 'job-sheets=confidential', 'job-sheets=secret', 'job-sheets=standard', 'job-sheets=topsecret', 'job-sheets=unclassified'], "\n")
    elseif a:CmdLine =~ '-o\s\+number-up-layout=\%(\w\|-\)*$'
	return join(['number-up-layout=btlr', 'number-up-layout=btrl', 'number-up-layout=lrbt', 'number-up-layout=lrtb', 'number-up-layout=rlbt', 'number-up-layout=rltb', 'number-up-layout=tblr', 'number-up-layout=tbrl'], "\n")
    elseif a:CmdLine =~ '-o\s\+position=\%(\w\|-\)*$'
	return join(['position=center', 'position=top', 'position=left', 'position=right', 'position=top-left', 'position=top-right', 
		    \ 'position=bottom', 'position=bottom-left', 'position=bottom-right'], "\n")
    endif
    return ""
endfunction
" }}}

" Open Library Command
" {{{ :Open
command! -nargs=? -bang -complete=file  Open call atplib#Open(<q-bang>, g:atp_LibraryPath, g:atp_OpenTypeDict, <q-args>)
let g:atp_open_completion = []
" -complete=customlist,ATP_CompleteOpen
" function! ATP_CompleteOpen(ArgLead, CmdLead, CurPos)
"     return filter(deepcopy(g:atp_open_completion), "v:val =~ '^' . a:ArgLead")
" endfunction
" }}}

" ToDo notes
" {{{ ToDo
"
" TODO if the file was not found ask to make one.
function! ToDo(keyword,stop,...)

    if a:0 == 0
	let bufname	= bufname("%")
    else
	let bufname	= a:1
    endif

    " read the buffer
    let texfile=getbufline(bufname, 1, "$")

    " find ToDos
    let todo = {}
    let nr=1
    for line in texfile
	if line =~ '%.*' . a:keyword 
	    call extend(todo, { nr : line }) 
	endif
	let nr += 1
    endfor

    " Show ToDos
    echohl atp_Todo
    if len(keys(todo)) == 0
	echomsg "[ATP:] list for '%.*" . a:keyword . "' in '" . bufname . "' is empty."
	return
    endif
    echomsg "[ATP:] list for '%.*" . a:keyword . "' in '" . bufname . "':"
    let sortedkeys=sort(keys(todo), "atplib#CompareNumbers")
    for key in sortedkeys
	" echo the todo line.
	echomsg key . " " . substitute(substitute(todo[key],'%','',''),'\t',' ','g')
	let true	= 1
	let a		= 1
	let linenr	= key
	" show all comment lines right below the found todo line.
	while true && texfile[linenr] !~ '%.*\c\<todo\>' 
	    let linenr=key+a-1
	    if texfile[linenr] =~ '\s*%' && texfile[linenr] !~ a:stop
		" make space of length equal to len(linenr)
		let space=""
		let j=0
		while j < len(linenr)
		    let space=space . " " 
		    let j+=1
		endwhile
		echomsg space . " " . substitute(substitute(texfile[linenr],'%','',''),'\t',' ','g')
	    else
		let true = 0
	    endif
	    let a += 1
	endwhile
    endfor
    echohl None
endfunction
" }}}

" This functions reloads ATP (whole or just a function)
" {{{  ReloadATP
" ReloadATP() - reload all the tex_atp functions and delete all autoload functions from
" autoload/atplib.vim
try
function! <SID>ReloadATP(bang)
    " First source the option file
    let common_file	= globpath(&rtp, 'ftplugin/ATP_files/common.vim')
    let options_file	= globpath(&rtp, 'ftplugin/ATP_files/options.vim')
    let g:atp_reload_functions = ( a:bang == "!" ? 1 : 0 ) 
    let g:atp_reload_variables = 0
    if a:bang == ""
	execute "source " . common_file
	execute "source " . options_file 

	" Then source atprc file
	if filereadable(globpath($HOME, '/.atprc.vim', 1)) && has("unix")

		" Note: in $HOME/.atprc file the user can set all the local buffer
		" variables without using autocommands
		let path = globpath($HOME, '/.atprc.vim', 1)
		execute 'source ' . fnameescape(path)

	else
		let path	= get(split(globpath(&rtp, "**/ftplugin/ATP_files/atprc.vim"), '\n'), 0, "")
		if path != ""
			execute 'source ' . fnameescape(path)
		endif
	endif
    else
	" Reload all functions and variables, 
	let tex_atp_file = globpath(&rtp, 'ftplugin/tex_atp.vim')
	execute "source " . tex_atp_file

	" delete functions from autoload/atplib.vim:
	let atplib_file	= globpath(&rtp, 'autoload/atplib.vim')
	let saved_loclist = getloclist(0)
	exe 'lvimgrep /^\s*fun\%[ction]!\=\s\+/gj '.atplib_file
	let list=map(getloclist(0), 'v:val["text"]')
	call setloclist(0,saved_loclist)
	call map(list, 'matchstr(v:val, ''^\s*fun\%[ction]!\=\s\+\zsatplib#\S\+\ze\s*('')')
	for fname in list
	    if fname != ""
		exe 'delfunction '.fname
	    endif
	endfor
    endif
    let g:atp_reload_functions 	= 0
    let g:atp_reload_variables  = 0
endfunction
catch /E127:/
    " Cannot redefine function, function is in use.
endtry
" }}}

" This functions prints preamble 
" {{{ Preambule
function! <SID>Preamble()
    let loclist = getloclist(0)
    let winview = winsaveview()
    exe '1lvimgrep /^[^%]*\\begin\s*{\s*document\s*}/j ' . fnameescape(b:atp_MainFile)
    let linenr = get(get(getloclist(0), 0, {}), 'lnum', 'nomatch')
    if linenr != 'nomatch'
	if expand("%:p") != atplib#FullPath(b:atp_MainFile)
	    let cfile = expand("%:p")

	    exe "keepalt edit " . b:atp_MainFile 
	endif
	exe "1," . (linenr-1) . "print"
	if exists("cfile")
	    exe "keepalt edit " . cfile
	endif
	call winrestview(winview)
    else	
	echomsg "[ATP:] not found \begin{document}."
    endif
endfunction
" }}}

" Get bibdata from ams
" {{{ AMSGet

try
function! <SID>GetAMSRef(what, bibfile)
    let what = substitute(a:what, '\s\+', ' ',	'g') 
    let what = substitute(what, '%',	'%25',	'g')
    let what = substitute(what, ',',	'%2C',	'g') 
    let what = substitute(what, ':',	'%3A',	'g')
    let what = substitute(what, ';',	'%3B',	'g')
    let what = substitute(what, '/',	'%2F',	'g')
    let what = substitute(what, '?',	'%3F',	'g')
    let what = substitute(what, '+',	'%2B',	'g')
    let what = substitute(what, '=',	'%3D',	'g')
    let what = substitute(what, '#',	'%23',	'g')
    let what = substitute(what, '\$',	'%24',	'g')
    let what = substitute(what, '&',	'%26',	'g')
    let what = substitute(what, '@',	'%40',	'g')
    let what = substitute(what, ' ',	'+',	'g')

 
    " Get data from AMS web site.
    let atpbib_WgetOutputFile = tempname()
    let URLquery_path = globpath(&rtp, 'ftplugin/ATP_files/url_query.py')
    if a:bibfile != "nobibfile"
	let url="http://www.ams.org/mathscinet-mref?ref=".what."&dataType=bibtex"
    else
	let url="http://www.ams.org/mathscinet-mref?ref=".what."&dataType=tex"
    endif
    let cmd=g:atp_Python." ".URLquery_path." ".shellescape(url)." ".shellescape(atpbib_WgetOutputFile)
    call system(cmd)
    let loclist = getloclist(0)

    try
	exe '1lvimgrep /\CNo Unique Match Found/j ' . fnameescape(atpbib_WgetOutputFile)
    catch /E480/
    endtry
    if len(getloclist(0))
	return ['NoUniqueMatch']
    endif

    if len(b:AllBibFiles) > 0
	let pattern = '@\%(article\|book\%(let\)\=\|conference\|inbook\|incollection\|\%(in\)\=proceedings\|manual\|masterthesis\|misc\|phdthesis\|techreport\|unpublished\)\s*{\|^\s*\%(ADDRESS\|ANNOTE\|AUTHOR\|BOOKTITLE\|CHAPTER\|CROSSREF\|EDITION\|EDITOR\|HOWPUBLISHED\|INSTITUTION\|JOURNAL\|KEY\|MONTH\|NOTE\|NUMBER\|ORGANIZATION\|PAGES\|PUBLISHER\|SCHOOL\|SERIES\|TITLE\|TYPE\|VOLUME\|YEAR\|MRCLASS\|MRNUMBER\|MRREVIEWER\)\s*=\s*.*$'
	try 
	    exe 'lvimgrep /'.pattern.'/j ' . fnameescape(atpbib_WgetOutputFile)
	catch /E480:/
	endtry
	let data = getloclist(0)
	call setloclist(0, loclist)

	if !len(data) 
	    echohl WarningMsg
	    echomsg "[ATP:] nothing found."
	    echohl None
	    return [0]
	endif

	let linenumbers = map(copy(data), 'v:val["lnum"]')
	let begin	= min(linenumbers)
	let end	= max(linenumbers)

	let bufnr = bufnr(atpbib_WgetOutputFile)
	" To use getbufline() buffer must be loaded. It is enough to use :buffer
	" command because vimgrep loads buffer and then unloads it. 
	execute "buffer " . bufnr
	let bibdata	= getbufline(bufnr, begin, end)
	execute "bdelete " . bufnr 
	let type = matchstr(bibdata[0], '@\%(article\|book\%(let\)\=\|conference\|inbook\|incollection\|\%(in\)\=proceedings\|manual\|masterthesis\|misc\|phdthesis\|techreport\|unpublished\)\ze\s*\%("\|{\|(\)')
        " Suggest Key:
	let bibkey = input("Provide a key (Enter for the AMS bibkey): ")
	if !empty(bibkey)
	    let bibdata[0] 	= type . '{' . bibkey . ','
	else
	    let bibdata[0] 	= substitute(matchstr(bibdata[0], '@\w*.*$'), '\(@\w*\)\(\s*\)', '\1', '')
	    " This will be only used to echomsg:
	    let bibkey	= matchstr(bibdata[0], '@\w*.\s*\zs[^,]*')
	endif
	call add(bibdata, "}")

	" Open bibfile and append the bibdata:
	execute "silent! edit " . a:bibfile
	if getline(line('$')) !~ '^\s*$' 
	    let bibdata = extend([''], bibdata)
	endif
	call append(line('$'), bibdata)
	normal GG
	echohl WarningMsg
	echomsg "[ATP:] bibkey " . bibkey . " appended to: " . a:bibfile 
	echohl Normal
    else
	" If the user is using \begin{bibliography} environment.
	let pattern = '^<tr><td align="left">'
	try 
	    exe 'lvimgrep /'.pattern.'/j ' . fnameescape(atpbib_WgetOutputFile)
	catch /E480:/
	endtry
	let data = getloclist(0)
	if !len(data) 
	    echohl WarningMsg
	    echomsg "[ATP:] nothing found."
	    echohl None
	    return [0]
	elseif len(data) > 1
	    echoerr "ATP Error: AMSRef vimgrep pattern error. You can send a bug report. Please include the exact :ATPRef command." 
	endif
	let bibref = '\bibitem{} ' . matchstr(data[0]['text'], '^<tr><td align="left">\zs.*\ze<\/td><\/tr>')
	let g:atp_bibref = bibref
	exe "let @" . g:atp_bibrefRegister . ' = "' . escape(bibref, '\') . '"'
	let bibdata = [ bibref ]
    endif
    let g:atp_bibdata = bibdata
    call delete(atpbib_WgetOutputFile)
    return bibdata
endfunction
catch /E127/
endtry

function! AMSRef(bang, what)
    if !exists("b:AllBibFiles")
	call FindInputFiles(b:atp_MainFile)
    endif
    if len(b:AllBibFiles) > 1
	let bibfile = inputlist(extend("Which bib file to use?", b:AllBibFiles))
    elseif len(b:AllBibFiles) == 1
	let bibfile = b:AllBibFiles[0]
    elseif !len(b:AllBibFiles)
	let bibfile = "nobibfile"
    endif

    let return=<SID>GetAMSRef(a:what, bibfile)
"     let g:bang=a:bang
"     let g:bibfile=bibfile
"     let g:return=return
    if a:bang == "" && bibfile != "nobibfile" && return != [0] && return != ['NoUniqueMatch']
	silent! w
	silent! bd
    elseif bibfile == "nobibfile" && return != [0] && return != ['NoUniqueMatch']
	redraw
	echohl WarningMsg
	echomsg "[ATP:] found bib data is in register " . g:atp_bibrefRegister
	echohl Normal
    elseif return[0] == 'NoUniqueMatch' 
	redraw
	echohl WarningMsg
	echomsg "[ATP:] no Unique Match Found"
	echohl None
    endif
endfunction
"}}}

" Count Words
" {{{ WordCount() ShowWordCount()
function! <SID>WordCount(bang)

    call atplib#write()

    let g:atp_WordCount = {}
    for file in keys(filter(copy(b:TypeDict), 'v:val == ''input''')) + [ b:atp_MainFile ]
	let wcount = substitute(system("detex -n " . fnameescape(file) . " | wc -w "), '\D', '', 'g')
	call extend(g:atp_WordCount, { file : wcount })
    endfor

    " sum values
    let val = values(g:atp_WordCount)
    let wc_sum = 0
    for i in val
	let wc_sum += i
    endfor

    return wc_sum
endfunction

function! <SID>ShowWordCount(bang)

    let wc = <SID>WordCount(a:bang)
    let c = 0
    if a:bang == "!"
	echo g:atp_WordCount[b:atp_MainFile] . "\t" . b:atp_MainFile
	for file in b:ListOfFiles
	    if get(g:atp_WordCount, file, "NOFILE") != "NOFILE"
		let c=1
		echo g:atp_WordCount[file] . "\t" . file 
	    endif
	endfor
	if c
	    echomsg wc
	endif
    else
	echomsg wc . "  " . b:atp_MainFile
    endif
endfunction "}}}

" Wdiff
" {{{
" Needs wdiff program.
function! <SID>Wdiff(new_file, old_file)

    if !executable("wdiff")
	echohl WarningMsg
	echo "You need to install GNU wdiff program." 
	echohl Normal
	return 1
    endif

    " Operate on temporary copies:
    try
	let new_file	= readfile(a:new_file)
    catch /E484/
	echohl ErrorMsg
	echomsg "[ATP:] can't open file " . a:new_file
	return 1
    endtry
    try
	let old_file	= readfile(a:old_file)
    catch /E484/
	echohl ErrorMsg
	echomsg "[ATP:] can't open file " . a:old_file
	return 1
    endtry

    " Remove the preamble:
    let new_pend=0
    for line in new_file
	if line =~ '^[^%]*\\begin{document}'
	    break
	endif
	let new_pend+=1
    endfor
    let old_pend=0
    for line in new_file
	if line =~ '^[^%]*\\begin{document}'
	    break
	endif
	let old_pend+=1
    endfor

    let new_preamble	= remove(new_file, 0, new_pend)  
    let old_preamble	= remove(old_file, 0, old_pend)  

"     let g:new_preamble = new_preamble
"     let g:old_preamble = old_preamble
"     let g:new_file	= new_file
"     let g:old_file	= old_file

    let new_tmp		= tempname()
    let old_tmp		= tempname()

    if new_preamble != old_preamble
	let which_pre=inputlist(["Wich preamble to use:", "(1) from " . a:new_file, "(2) from " . a:old_file])
	if which_pre != 1 && which_pre != 2
	    return 0
	endif
    else
	let which_pre = 1
    endif

    execute "keepalt edit " . new_tmp
    call append(0, new_file)
    let buf_new	= bufnr("%")
    "delete all comments
    if expand("%:p") == new_tmp
	silent! execute ':%g/^\s*%/d'
	silent! execute ':%s/\s*\\\@!%.*$//g'
	silent! write
	silent! bdelete
    else
	return 1
    endif

    execute "keepalt edit " . old_tmp
    call append(0, old_file)
    let buf_old	= bufnr("%")
    "delete all comments
    if expand("%:p") == old_tmp
	silent! execute ':%g/^\s*%/d'
	silent! execute ':%s/\s*\\\@!%.*$//g'
	silent! write
	silent! bdelete
    else
	return 1
    endif

    " make wdiff:
    if filereadable("/tmp/wdiff.tex")
	call delete("/tmp/wdiff.tex")
    endif
"     call system("wdiff -w '{\\textcolor{red}{=}' -x '\\textcolor{red}{=}}' -y '{\\textcolor{blue}{+}' -z '\\textcolor{blue}{+}}' " . new_tmp . " " . old_tmp . " > /tmp/wdiff.tex")
    call system("wdiff " . "-w '\\{=' -x '=\\}' -y '\\{+' -z '+\\}'" . " " . new_tmp . " " . old_tmp . " > /tmp/wdiff.tex")
    split /tmp/wdiff.tex

    " Set atp
    let b:atp_autex=0
    let b:atp_ProjectScript=0

    " These do not match multiline changes!
    let s:atp_IDdelete	= matchadd('DiffDelete', '\\{=\zs\%(=\\}\@!\|=\\\@!}\|=\@!\\}\|[^}=\\]\|=\\\@!\|\\}\@!\|=\@<!\\\|\\}\@!\|\\\@<!}\)*\ze=\\}', 10)
    let s:atp_IDadd	= matchadd('DiffAdd', '\\{+\zs\%(+\\}\@!\|+\\\@!}\|+\@!\\}\|[^}+\\]\|+\\\@!\|\\}\@!\|+\@<!\\\|\\}\@!\|\\\@<!}\)*\ze+\\}', 10)
    normal "gg"
    call append(0, ( which_pre == 1 ? new_preamble : old_preamble )) 
    silent! call search('\\begin{document}')
    normal "zt"
    map ]s /\\{[=+]\_.*[+=]\\}<CR>
    map [s ?\\{[=+]\_.*[+=]\\}<CR>
    function! NiceDiff()
	let saved_pos=getpos(".")
	keepjumps %s/\\{=\(\%(=\\}\@!\|=\\\@!}\|=\@!\\}\|[^}=\\]\|=\\\@!\|\\}\@!\|=\@<!\\\|\\}\@!\|\\\@<!}\)*\)=\\}/\\textcolor{red}{\1}/g
	keepjumps %s/\\{+\(\%(+\\}\@!\|+\\\@!}\|+\@!\\}\|[^}+\\]\|+\\\@!\|\\}\@!\|+\@<!\\\|\\}\@!\|\\\@<!}\)*\)+\\}/\\textcolor{blue}{\1}/g
	call cursor(saved_pos[1], saved_pos[2])
	map ]s /\\textcolor{\%(blue\|red\)}{/e+1
	map [s ?\\textcolor{\%(blue\|red\)}{?e+1
	call matchadd('DiffDelete', '\textcolor{red}{[^}]*}', 10)
	call matchadd('DiffAdd', '\textcolor{blue}{[^}]*}',  10)
    endfunction
    command! -buffer NiceDiff :call NiceDiff()
endfunction "}}}

" ATPUpdate
try "{{{ UpdateATP
function! <SID>UpdateATP(bang)
    "DONE: add bang -> get stable/unstable latest release.
    "DONE: check if the current version is newer than the available one
    "		if not do not download and install (this saves time).

	if g:atp_debugUpdateATP
	    exe "redir! > ".g:atp_TempDir."/UpdateATP.log"
	endif
	let s:ext = "tar.gz"
	if a:bang == "!"
	    echo "[ATP:] getting list of available snapshots ..."
	else
	    echo "[ATP:] getting list of available versions ..."
	endif
	let s:URLquery_path = globpath(&rtp, 'ftplugin/ATP_files/url_query.py')    

	if a:bang == "!"
	    let url = "http://sourceforge.net/projects/atp-vim/files/snapshots/"
	else
	    let url = "http://sourceforge.net/projects/atp-vim/files/releases/"
	endif
	let url_tempname=tempname()."_ATP.html"
	let cmd=g:atp_Python." ".s:URLquery_path." ".shellescape(url)." ".shellescape(url_tempname)
	if g:atp_debugUpdateATP
	    let g:cmd=cmd
	    silent echo "url_tempname=".url_tempname
	    silent echo "cmd=".cmd
	endif
	call system(cmd)

	let saved_loclist = getloclist(0)
	exe 'lvimgrep /\C<a\s\+href=".*AutomaticTexPlugin_\d\+\%(\.\d\+\)*\.'.escape(s:ext, '.').'/jg '.url_tempname
	call delete(url_tempname)
	let list = map(getloclist(0), 'v:val["text"]')
	if g:atp_debugUpdateATP
	    silent echo "list=".string(list)
	endif
	if a:bang == "!"
	    call filter(list, 'v:val =~ ''\.tar\.gz\.\d\+-\d\+-\d\+_\d\+-\d\+''')
	endif
	call map(list, 'matchstr(v:val, ''<a\s\+href="\zshttp[^"]*download\ze"'')')
	call setloclist(0,saved_loclist)
	call filter(list, "v:val != ''")
	if g:atp_debugUpdateATP
	    silent echo "atp_versionlist=".string(list)
	endif

	if !len(list)
	    echoerr "No snapshot is available." 
	    if g:atp_debugUpdateATP
		redir END
	    endif
	    return
	endif
	let dict = {}
	for item in list
	    if a:bang == "!"
		let key = matchstr(item, 'AutomaticTexPlugin_\d\+\%(\.\d\+\)*\.tar\.gz\.\zs[\-0-9_]\+\ze')
		if key == ''
		    let key = "00-00-00_00-00"
		endif
		call extend(dict, { key : item})
	    else
		call extend(dict, { matchstr(item, 'AutomaticTexPlugin_\zs\d\+\%(\.\d\+\)*\ze\.tar.gz') : item})
	    endif
	endfor
	if a:bang == "!"
	    let sorted_list = sort(keys(dict), "<SID>CompareStamps")
	else
	    let sorted_list = sort(keys(dict), "<SID>CompareVersions")
	endif
	if g:atp_debugUpdateATP
	    silent echo "dict=".string(dict)
	    silent echo "sorted_list=".string(sorted_list)
	endif
	"NOTE: this list might contain one item two times (I'm not filtering well the
	" html sourcefore web page, but this is faster)

	let dir = fnamemodify(globpath(&rtp, "ftplugin/tex_atp.vim"), ":h:h")
	if dir == ""
	    echoerr "[ATP:] Cannot find local .vim directory."
	    if g:atp_debugUpdateATP
		redir END
	    endif
	    return
	endif

	" Stamp of the local version
	let saved_loclist = getloclist(0)
	if a:bang == "!"
	    try
		exe '1lvimgrep /\C^"\s*Time\s\+Stamp:/gj '. globpath(&rtp, "ftplugin/tex_atp.vim")
		let old_stamp = get(getloclist(0),0, {'text' : '00-00-00_00-00'})['text']
		call setloclist(0, saved_loclist) 
		let old_stamp=matchstr(old_stamp, '^"\s*Time\s\+Stamp:\s*\zs\%(\d\|_\|-\)*\ze')
	    catch /E480:/
		let old_stamp="00-00-00_00-00"
	    endtry
	else
	    try
		exe '1lvimgrep /(ver\.\=\%[sion]\s\+\d\+\%(\.\d\+\)*\s*)/gj ' . globpath(&rtp, "doc/automatic-tex-plugin.txt")
		let old_stamp = get(getloclist(0),0, {'text' : '00-00-00_00-00'})['text']
		call setloclist(0, saved_loclist) 
		let old_stamp=matchstr(old_stamp, '(ver\.\=\%[sion]\s\+\zs\d\+\%(\.\d\+\)*\ze')
	    catch /E480:/
		let old_stamp="0.0"
	    endtry
	endif
	if g:atp_debugUpdateATP
	    silent echo "old_stamp=".old_stamp
	endif

	function! <SID>GetLatestSnapshot(bang,url)
	    " Get latest snapshot/version
	    let url = a:url

	    let s:ATPversion = matchstr(url, 'AutomaticTexPlugin_\zs\d\+\%(\.\d\+\)*\ze\.'.escape(s:ext, '.'))
	    if a:bang == "!"
		let ATPdate = matchstr(url, 'AutomaticTexPlugin_\d\+\%(\.\d\+\)*.'.escape(s:ext, '.').'.\zs[0-9-_]*\ze')
	    else
		let ATPdate = ""
	    endif
	    let s:atp_tempname = tempname()."_ATP.tar.gz"
	    if g:atp_debugUpdateATP
		silent echo "tempname=".s:atp_tempname
	    endif
	    let cmd=g:atp_Python." ".s:URLquery_path." ".shellescape(url)." ".shellescape(s:atp_tempname)
	    let g:get_cmd=cmd
	    if a:bang == "!"
		echo "[ATP:] getting latest snapshot (unstable version) ..."
	    else
		echo "[ATP:] getting latest stable version ..."
	    endif
	    if g:atp_debugUpdateATP
		silent echo "cmd=".cmd
	    endif
	    call system(cmd)
	endfunction

	let new_stamp = sorted_list[0]
	if g:atp_debugUpdateATP
	    silent echo "new_stamp=".new_stamp
	endif
	 
	"Compare stamps:
	" stamp format day-month-year_hour-minute
	" if o_stamp is >= than n_stamp  ==> return
	let l:return = 1
	if a:bang == "!"
	    let compare = <SID>CompareStamps(new_stamp, old_stamp)
	else
	    let compare = <SID>CompareVersions(new_stamp, old_stamp) 
	endif
	if a:bang == "!"
	    if  compare == 1 || compare == 0
		redraw
		echomsg "You have the latest UNSTABLE version of ATP."
		if g:atp_debugUpdateATP
		    redir END
		endif
		return
	    endif
	else
	    if compare == 1
		redraw
		let l:return = input("You have UNSTABLE version of ATP.\nDo you want to DOWNGRADE to the last STABLE release? type yes/no [or y/n] and hit <Enter> ")
		let l:return = (l:return !~? '^\s*y\%[es]\s*$')
		if l:return
		    call delete(s:atp_tempname)
		    redraw
		    if g:atp_debugUpdateATP
			redir END
		    endif
		    return
		endif
	    elseif compare == 0
		redraw
		echomsg "You have the latest STABLE version of ATP."
		if g:atp_debugUpdateATP
		    redir END
		endif
		return
	    endif
	endif

	redraw
	call  <SID>GetLatestSnapshot(a:bang, dict[sorted_list[0]])
	echo "[ATP:] installing ..." 
	call <SID>Tar(s:atp_tempname, dir)
	call delete(s:atp_tempname)

	exe "helptags " . finddir("doc", dir)
	ReloadATP
	redraw!
	if a:bang == "!"
	    echomsg "[ATP:] updated to version ".s:ATPversion." (snapshot date stamp ".new_stamp.")." 
	    echo "See ':help atp-news' for changes!"
	else
	    echomsg "[ATP:] ".(l:return ? 'updated' : 'downgraded')." to release ".s:ATPversion
	endif
	if bufloaded(globpath(&rtp, "doc/automatic-tex-plugin.txt")) ||
		    \ bufloaded(globpath(&rtp, "doc/bibtex_atp.txt"))
	    echo "[ATP:] to reload the ATP help files (and see what's new!), close and reopen them."
	endif
endfunction 
catch E127:
endtry
function! <SID>CompareStamps(new, old)
    " newer stamp is smaller 
    " vim sort() function puts smaller items first.
    " new > old => -1
    " new = old => 0
    " new < old => 1
    let new=substitute(a:new, '\.', '', 'g')
    let old=substitute(a:old, '\.', '', 'g')
    return ( new == old ? 0 : new > old ? -1 : 1 )
endfunction
function! <SID>CompareVersions(new, old)
    " newer stamp is smaller 
    " vim sort() function puts smaller items first.
    " new > old => -1
    " new = old => 0
    " new < old => 1
    let new=split(a:new, '\.')
    let old=split(a:old, '\.')
    let g:new=new
    let g:old=old
    let compare = []
    for i in range(max([len(new), len(old)]))
	let nr = (get(new,i,0) < get(old,i,0) ? 1 : ( get(new,i,0) == get(old,i,0) ? 0 : 2 ))
	call add(compare, nr)
    endfor
    let comp = join(compare, "")
    " comp =~ '^0*1' new is older version 
    return ( comp == 0 ? 0 : ( comp =~ '^0*1' ? 1 : -1 ))

"     return ( new == old ? 0 : new > old ? -1 : 1 )
endfunction
function! <SID>GetTimeStamp(file)
python << END
import vim, tarfile, re

file_name	=vim.eval('a:file')
tar_file	=tarfile.open(file_name, 'r:gz')
def tex(name):
    if re.search('ftplugin/tex_atp\.vim', str(name)):
	return True
    else:
	return False
member=filter(tex, tar_file.getmembers())[0]
pfile=tar_file.extractfile(member)
stamp=""
for line in pfile.readlines():
    if re.match('\s*"\s+Time\s+Stamp:\s+', line):
	stamp=line
	break
try:
    match=re.match('\s*"\s+Time\s+Stamp:\s+([0-9\-_]*)', stamp)
    stamp=match.group(1)
except AttributeError:
    stamp="00-00-00_00-00"
vim.command("let g:atp_stamp='"+stamp+"'")
END
endfunction
function! <SID>Tar(file,path)
python << END
import tarfile, vim
file_n=vim.eval("a:file")
path=vim.eval("a:path")
file_o=tarfile.open(file_n, "r:gz")
file_o.extractall(path)
END
endfunction
" function! Tar(file,path)
" python << END
" import tarfile, vim
" file_n=vim.eval("a:file")
" print(file_n)
" path=vim.eval("a:path")
" print(path)
" file_o=tarfile.open(file_n, "r:gz")
" file_o.extractall(path)
" END
" endfunction
function! <SID>ATPversion()
    " This function is used in opitons.vim
    let saved_loclist = getloclist(0)
    try
	exe 'lvimgrep /\C^"\s*Time\s\+Stamp:/gj '. globpath(&rtp, "ftplugin/tex_atp.vim")
	let stamp 	= get(getloclist(0),0, {'text' : '00-00-00_00-00'})['text']
	let stamp	= matchstr(stamp, '^"\s*Time\s\+Stamp:\s*\zs\%(\d\|_\|-\)*\ze')
    catch /E480:/
	let stamp	= "(no stamp)"
    endtry
    try
	exe 'lvimgrep /^\C\s*An\s\+Introduction\s\+to\s\+AUTOMATIC\s\+(La)TeX\s\+PLUGIN\s\+(ver\%(\.\|sion\)\=\s\+[0-9.]*)/gj '. globpath(&rtp, "doc/automatic-tex-plugin.txt")
	let l:version = get(getloclist(0),0, {'text' : 'unknown'})['text']
	let l:version = matchstr(l:version, '(ver\.\?\s\+\zs[0-9.]*\ze)')
    catch /E480:/
	let l:version = "(no version number)"
    endtry
    call setloclist(0, saved_loclist) 
    redraw
    let g:atp_version = l:version ." (".stamp.")" 
    return "ATP version: ".l:version.", time stamp: ".stamp."."
endfunction
"}}}

" Comment Lines
function! Comment(arg) "{{{

    " remember the column of the cursor
    let col=col('.')
     
    if a:arg==1
	call setline(line('.'),g:atp_CommentLeader . getline('.'))
	let l:scol=l:col+len(g:atp_CommentLeader)-4
	call cursor(line('.'),l:scol)
    elseif a:arg==0 && getline('.') =~ '^\s*' . g:atp_CommentLeader
	call setline(line('.'),substitute(getline('.'),g:atp_CommentLeader,'',''))
	call cursor(line('.'),l:col-len(g:atp_CommentLeader))
    endif

endfunction "}}}

" DebugPrint
" cat files under g:atp_TempDir (with ATP debug info)
" {{{
if has("unix") && g:atp_atpdev
    function! <SID>DebugPrint(file)
	if a:file == ""
	    return
	endif
	let dir = getcwd()
	exe "lcd ".g:atp_TempDir
	if filereadable(a:file)
	    echo join(readfile(a:file), "\n")
	else
	    echomsg "No such file."
	endif
	exe "lcd ".escape(dir, ' ')
    endfunction
    function! <SID>DebugPrintComp(A,C,L)
	let list = split(globpath(g:atp_TempDir, "*"), "\n")
	let dir = getcwd()
	exe "lcd ".g:atp_TempDir
	call map(list, "fnamemodify(v:val, ':.')")
	exe "lcd ".escape(dir, ' ')
	return join(list, "\n")
    endfunction
    command! -nargs=? -complete=custom,<SID>DebugPrintComp DebugPrint	:call <SID>DebugPrint(<q-args>)
endif "}}}
endif "}}}


" COMMANDS AND MAPS:
" Maps: "{{{1
map <buffer> <Plug>CommentLines					:call Comment(1)<CR>
map <buffer> <Plug>UnCommentLines 				:call Comment(0)<CR>
vmap <buffer> 	<Plug>WrapSelection				:<C-U>call <SID>WrapSelection('')<CR>i
vmap <buffer> 	<Plug>InteligentWrapSelection			:<C-U>call <SID>InteligentWrapSelection('')<CR>i
nnoremap <silent> <buffer> 	<Plug>ToggleStar		:call <SID>ToggleStar()<CR>
nnoremap <silent> <buffer> 	<Plug>ToggleEnvForward		:call <SID>ToggleEnvironment(0, 1)<CR>
nnoremap <silent> <buffer> 	<Plug>ToggleEnvBackward		:call <SID>ToggleEnvironment(0, -1)<CR>
nnoremap <silent> <buffer> 	<Plug>ChangeEnv			:call <SID>ToggleEnvironment(1)<CR>
nnoremap <silent> <buffer> 	<Plug>TexDoc			:TexDoc 
" Commands: "{{{1
command! -buffer -nargs=* -complete=file Wdiff			:call <SID>Wdiff(<f-args>)
command! -buffer -nargs=* -range WrapSelection			:call <SID>WrapSelection(<f-args>)
command! -buffer -nargs=? -complete=customlist,EnvCompletion -range WrapEnvironment		:call <SID>WrapEnvironment(<f-args>)
command! -buffer -nargs=? -range InteligentWrapSelection	:call <SID>InteligentWrapSelection(<args>)
command! -buffer	TexAlign				:call TexAlign()
command! -buffer 	ToggleStar   				:call <SID>ToggleStar()<CR>
command! -buffer -nargs=? ToggleEnv	   			:call <SID>ToggleEnvironment(0, <f-args>)
command! -buffer -nargs=* -complete=customlist,EnvCompletion ChangeEnv				:call <SID>ToggleEnvironment(1, <f-args>)
command! -buffer -nargs=* -complete=customlist,<SID>TeXdoc_complete TexDoc 	:call <SID>TexDoc(<f-args>)
command! -buffer -bang 	Delete					:call <SID>Delete(<q-bang>)
nmap <silent> <buffer>	 <Plug>Delete				:call <SID>Delete("")<CR>
command! -buffer 	OpenLog					:call <SID>OpenLog()
nnoremap <silent> <buffer> <Plug>OpenLog			:call <SID>OpenLog()<CR>
command! -buffer 	TexLog					:call <SID>TexLog()
nnoremap <silent> <buffer> <Plug>TexLog				:call <SID>TexLog()<CR>
command! -buffer 	PdfFonts				:call <SID>PdfFonts()
nnoremap <silent> <buffer> <Plug>PdfFonts			:call <SID>PdfFonts()<CR>
command! -complete=custom,s:Complete_lpr  -buffer -nargs=* SshPrint 	:call <SID>SshPrint("", <f-args>)
command! -complete=custom,s:CompleteLocal_lpr  -buffer -nargs=* Lpr	:call <SID>Lpr(<f-args>)
nnoremap <buffer> 	<Plug>SshPrint				:SshPrint 
command! -buffer 	Lpstat					:call <SID>Lpstat()
nnoremap <silent> <buffer> <Plug>Lpstat				:call <SID>Lpstat()<CR>
command! -buffer 	ListPrinters				:echo <SID>ListPrinters("", "", "")
" List Packages:
command! -buffer 	ShowPackages				:let b:atp_PackageList = atplib#GrepPackageList() | echo join(b:atp_PackageList, "\n")
if &l:cpoptions =~# 'B'
    command! -buffer -nargs=? -complete=buffer ToDo			:call ToDo('\c\<to\s*do\>','\s*%\c.*\<note\>',<f-args>)
    command! -buffer -nargs=? -complete=buffer Note			:call ToDo('\c\<note\>','\s*%\c.*\<to\s*do\>',<f-args>)
else
    command! -buffer -nargs=? -complete=buffer ToDo			:call ToDo('\\c\\<to\\s*do\\>','\\s*%\\c.*\\<note\\>',<f-args>)
    command! -buffer -nargs=? -complete=buffer Note			:call ToDo('\\c\\<note\\>','\\s*%\\c.*\\<to\\s*do\\>',<f-args>)
endif
command! -buffer ReloadATP					:call <SID>ReloadATP("!")
command! -bang -buffer -nargs=1 AMSRef				:call AMSRef(<q-bang>, <q-args>)
command! -buffer	Preamble				:call <SID>Preamble()
command! -bang		WordCount				:call <SID>ShowWordCount(<q-bang>)
command! -buffer -bang	UpadteATP				:call <SID>UpdateATP(<q-bang>)
command! -buffer	ATPversion				:echo <SID>ATPversion()
" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
