" Author:	Marcin Szamotulski
" Description:	This file contains motion and highlight functions of ATP.
" Note:		This file is a part of Automatic Tex Plugin for Vim.
" Language:	tex
" Last Change:

let s:sourced = ( !exists("s:sourced") ? 0 : 1 )

" Functions: (source once)
if !s:sourced || g:atp_reload_functions "{{{1
" All table  of contents stuff: variables, functions and commands. 
" {{{2 Table Of Contents
"--Make TOC -----------------------------
" This makes sense only for latex documents.
"
" Notes: Makeing toc from aux file:
" 	+ is fast
" 	+ one gets correct numbers
" 	- one doesn't get line numbers
" 		/ the title might be modified thus one can not make a pattern
" 		    which works in all situations, while this is important for 
" 		    :DeleteSection command /
"
" {{{3 s:find_toc_lines
function! s:find_toc_lines()
    let toc_lines_nr=[]
    let toc_lines=[]

    let pos_saved=getpos(".")
    let pos=[0,1,1,0]
    keepjumps call setpos(".",pos)

    " Pattern:
    let j=0
    for section in keys(g:atp_sections)
	if j == 0 
	    let pattern=g:atp_sections[section][0] . ''
	else
	    let pattern=pattern . '\|' . g:atp_sections[section][0] 
	endif
	let j+=1
    endfor

    " Searching Loop:
    let line=search(pattern, 'W')
    while line
	call add(toc_lines_nr, line)
	let line=search(pattern, 'W')
    endwhile
    keepjumps call setpos(".", pos_saved)
    for line in toc_lines_nr
	call add(toc_lines, getline(line))
    endfor
    return toc_lines
endfunction
" {{{3 s:maketoc 
" this will store information: 
" { 'linenumber' : ['chapter/section/..', 'sectionnumber', 'section title', '0/1=not starred/starred'] }
function! s:maketoc(filename)
    let toc={}

    " if the dictinary with labels is not defined, define it
    if !exists("t:atp_labels")
	let t:atp_labels = {}
    endif

    let texfile		= []
    " getbufline reads only loaded buffers, unloaded can be read from file.
    let bufname		= fnamemodify(a:filename,":t")
    try
	let texfile = ( bufexists(bufname)  ? getbufline("^" . bufname . "$","1","$") : readfile(a:filename) )
    catch /E484:/
	echohl Warning
	echo "File " . a:filename . " not readable."
    endtry
    let texfile_copy	= deepcopy(texfile)

    let true		= 1
    let bline		= 0 	" We are not removing the preambule any more.
    let i		= 1
    " set variables for chapter/section numbers
    for section in keys(g:atp_sections)
	let ind{section} = 0
    endfor
    " make a filter
    let j = 0
    let biblatex	= ( atplib#SearchPackage("biblatex") )
    " When \usepackge{biblatex} do not search for \bibliography{} commands -- they are placed in ther preambule.
    let key_list 	= ( biblatex ? filter(keys(g:atp_sections), "v:val != 'bibliography'") : keys(g:atp_sections) ) 
    for section in key_list
	let filter = ( j == 0 ? g:atp_sections[section][0] . '' : filter . '\|' . g:atp_sections[section][0] )
	let j+=1
    endfor

    let s:filtered	= filter(deepcopy(texfile), 'v:val =~ filter')
    for line in s:filtered
	for section in keys(g:atp_sections)
	    if line =~ g:atp_sections[section][0] 
		if line !~ '^\s*\\\@<!%'
		    " THIS DO NOT WORKS WITH \abstract{ --> empty set, but with
		    " \chapter{title} --> title, solution: the name of
		    " 'Abstract' will be plased, as we know what we have
		    " matched
		    let title	= line

		    " test if it is a starred version.
		    let star=0
		    if g:atp_sections[section][1] != 'nopattern' && line =~ g:atp_sections[section][1] 
			let star=1 
		    else
			let star=0
		    endif

		    " Problem: If there are two sections with the same title, this
		    " does't work:
		    let idx	= index(texfile,line)
		    call remove(texfile, idx)
		    let i	= idx
		    let tline	= i+bline+1
		    let bline	+=1

		    " Find Title:
		    let start	= stridx(title,'{')+1
		    let title	= strpart(title,start)
		    " we are looking for the maching '}' 
		    let l:count	= 1
		    let i=-1
		    while i<=len(title)
			let i+=1
			if strpart(title,i,1) == '{'	
			    let l:count+=1
			elseif strpart(title,i,1) == '}'
			    let l:count-=1
			endif
			if l:count == 0
			    break
			endif
		    endwhile	
		    let title = strpart(title,0,i)

		    " Section Number:
		    " if it is not starred version add one to the section number
		    " or it is not an abstract 
		    if star == 0  
			if !(section == 'chapter' && title =~ '^\cabstract$')
			    let ind{section}+=1
			endif
		    endif

		    if section == 'part'
			let indchapter		= 0
			let indsection		= 0
			let indsubsection	= 0
			let indsubsubsection	= 0
		    elseif section ==  'chapter'
			let indsection		= 0
			let indsubsection	= 0
			let indsubsubsection	= 0
		    elseif section ==  'section'
			let indsubsection	= 0
			let indsubsubsection	= 0
		    elseif section ==  'subsection'
			let indsubsubsection	= 0
		    endif

		    " Find Short Title:
		    let shorttitle=line
		    let start=stridx(shorttitle,'[')+1
		    if start == 0
			let shorttitle=''
		    else
			let shorttitle=strpart(shorttitle,start)
			" we are looking for the maching ']' 
			let l:count=1
			let i=-1
			while i<=len(shorttitle)
			    let i+=1
			    if strpart(shorttitle,i,1) == '['	
				let l:count+=1
			    elseif strpart(shorttitle,i,1) == ']'
				let l:count-=1
			    endif
			    if l:count==0
				break
			    endif
			endwhile	
			let shorttitle = strpart(shorttitle,0,i)
		    endif

		    "ToDo: if section is bibliography (using bib) then find the first
		    " empty line:
		    if section == "bibliography" && line !~ '\\begin\s*{\s*thebibliography\s*}' && !biblatex
			let idx	= tline-1
			while texfile_copy[idx] !~ '^\s*$'
			    let idx-= 1
			endwhile
" 			" We add 1 as we want the first non blank line, and one more
" 			" 1 as we want to know the line number not the list index
" 			" number:
			let tline=idx+1
		    endif

		    " Add results to the dictionary:
		    if biblatex && section != "bibliography" || !biblatex
			call extend(toc, { tline : [ section, ind{section}, title, star, shorttitle] }) 
		    endif

		endif
	    endif
	endfor
    endfor
"     if exists("t:atp_toc")
" 	call extend(t:atp_toc, { a:filename : toc }, "force")
"     else
" 	let t:atp_toc = { a:filename : toc }
"     endif
"     return t:atp_toc
    return { a:filename : toc }
endfunction
" {{{3 s:buflist
if !exists("t:atp_toc_buflist")
    let t:atp_toc_buflist=[]
endif
function! s:buflist()
    " this names are used in TOC and passed to s:maketoc, which
    " makes a dictionary whose keys are the values of name defined
    " just below:
    if !exists("t:atp_toc_buflist")
	let t:atp_toc_buflist = []
    endif
    let name=resolve(fnamemodify(bufname("%"),":p")) " add an entry to the list t:atp_toc_buflist if it is not there.
    if bufname("") =~ ".tex" && index(t:atp_toc_buflist,name) == -1
	call add(t:atp_toc_buflist,name)
    endif
    return t:atp_toc_buflist
endfunction
" {{{3 RemoveFromBufList
function! RemoveFromToC(file)
    if a:file == ""
	let g:debug = 1
	if exists("b:atp_MainFile")
	    let list = filter(copy(t:atp_toc_buflist), "v:val != fnamemodify(b:atp_MainFile, ':p')")
	else
	    let list = copy(t:atp_toc_buflist)
	endif
	if len(list) >= 2
	    let i=1
	    for f in list
		echo "(" . i . ") " . f
		let i+=1
	    endfor
	    let which=input("Which file to remove (press <Enter> for none)")
	    if which == ""
		let g:debug=3
		return
	    endif
	    let which=t:atp_toc_buflist[which-1]
	elseif exists("b:atp_MainFile") && len(list) == 1
	    let which=get(list,0,"")
	else
	    return
	endif
    else
	let which = fnamemodify(a:file, ":p")
    endif
    let g:which = which

    if which != ""
	silent! call remove(t:atp_toc_buflist,index(t:atp_toc_buflist, which))
	silent! call remove(t:atp_toc,which)
    endif
    let winnr=winnr()
    call <SID>TOC("!", 0)
    exe winnr."wincmd w"
endfunction
function! RemoveFromToCComp(A, B, C)
    if exists("b:atp_MainFile")
	let list = filter(copy(t:atp_toc_buflist), "v:val != fnamemodify(b:atp_MainFile, ':p')")
    else
	let list = copy(t:atp_toc_buflist)
    endif
    return join(list,"\n")
endfunction
" {{{3 s:showtoc
function! s:showtoc(toc)

    " this is a dictionary of line numbers where a new file begins.
    let cline=line(".")
"     " Open new window or jump to the existing one.
"     " Remember the place from which we are coming:
"     let t:atp_bufname=bufname("")
"     let t:atp_winnr=winnr()	 these are already set by TOC()
    let bname="__ToC__"
    let tocwinnr=bufwinnr(bufnr("^".bname."$"))
    if tocwinnr != -1
	" Jump to the existing window.
	    exe tocwinnr . " wincmd w"
	    silent exe "%delete"
    else
	" Open new window if its width is defined (if it is not the code below
	" will put toc in the current buffer so it is better to return.
	if !exists("t:toc_window_width")
	    let t:toc_window_width = g:atp_toc_window_width
	endif
	let openbuffer="keepalt " . t:toc_window_width . "vsplit +setl\\ wiw=15\\ buftype=nofile\\ nobuflisted\\ tabstop=1\\ filetype=toc_atp\\ nowrap __ToC__"
	keepalt silent exe  openbuffer
	" We are setting the address from which we have come.
	silent call atplib#setwindow()
    endif
    let number=1
    " this is the line number in ToC.
    " number is a line number relative to the file listed in ToC.
    " the current line number is linenumber+number
    " there are two loops: one over linenumber and the second over number.
    let numberdict	= {}
    let s:numberdict	= numberdict
    unlockvar b:atp_Toc
    let b:atp_Toc	= {}
    " this variable will be used to set the cursor position in ToC.
    for openfile in keys(a:toc)
	call extend(numberdict, { openfile : number })
	let part_on=0
	let chap_on=0
	let chnr=0
	let secnr=0
	let ssecnr=0
	let sssecnr=0
	let path=fnamemodify(bufname(""),":p:h")
	for line in keys(a:toc[openfile])
	    if a:toc[openfile][line][0] == 'chapter'
		let chap_on=1
		break
	    elseif a:toc[openfile][line][0] == 'part'
		let part_on=1
	    endif
	endfor
	let sorted	= sort(keys(a:toc[openfile]), "atplib#CompareNumbers")
	let len		= len(sorted)
	" write the file name in ToC (with a full path in paranthesis)
	call setline(number,fnamemodify(openfile,":t") . " (" . fnamemodify(openfile,":p:h") . ")")
	call extend(b:atp_Toc, { number : [ openfile, 1 ]}) 
	let number+=1
	for line in sorted
	    call extend(b:atp_Toc,  { number : [ openfile, line ] })
	    let lineidx=index(sorted,line)
	    let nlineidx=lineidx+1
	    if nlineidx< len(sorted)
		let nline=sorted[nlineidx]
	    else
		let nline=line("$")
	    endif
	    let lenght=len(line) 	
	    if lenght == 0
		let showline="     "
	    elseif lenght == 1
		let showline="    " . line
	    elseif lenght == 2
		let showline="   " . line
	    elseif lenght == 3
		let showline="  " . line
	    elseif lenght == 4
		let showline=" " . line
	    elseif lenght>=5
		let showline=line
	    endif
	    " Print ToC lines.
	    if a:toc[openfile][line][0] == 'abstract' || a:toc[openfile][line][2] =~ '^\cabstract$'
		call setline(number, showline . "\t" . "  " . "Abstract" )
	    elseif a:toc[openfile][line][0] =~ 'bibliography\|references'
		call setline (number, showline . "\t" . "  " . a:toc[openfile][line][2])
	    elseif a:toc[openfile][line][0] == 'part'
		let partnr=a:toc[openfile][line][1]
		let nr=partnr
		if a:toc[openfile][line][3]
		    "if it is stared version
		    let nr=substitute(nr,'.',' ','')
		endif
		if a:toc[openfile][line][4] != ''
" 		    call setline (number, showline . "\t" . nr . " " . a:toc[openfile][line][4])
		    call setline (number, showline . "\t" . " " . a:toc[openfile][line][4])
		else
" 		    call setline (number, showline . "\t" . nr . " " . a:toc[openfile][line][2])
		    call setline (number, showline . "\t" . " " . a:toc[openfile][line][2])
		endif
	    elseif a:toc[openfile][line][0] == 'chapter'
		let chnr=a:toc[openfile][line][1]
		let nr=chnr
		if a:toc[openfile][line][3]
		    "if it is stared version
		    let nr=substitute(nr,'.',' ','')
		endif
		if a:toc[openfile][line][4] != ''
		    call setline (number, showline . "\t" . nr . " " . a:toc[openfile][line][4])
		else
		    call setline (number, showline . "\t" . nr . " " . a:toc[openfile][line][2])
		endif
	    elseif a:toc[openfile][line][0] == 'section'
		let secnr=a:toc[openfile][line][1]
		if chap_on
		    let nr=chnr . "." . secnr  
		    if a:toc[openfile][line][3]
			"if it is stared version
			let nr=substitute(nr,'.',' ','g')
		    endif
		    if a:toc[openfile][line][4] != ''
			call setline (number, showline . "\t\t" . nr . " " . a:toc[openfile][line][4])
		    else
			call setline (number, showline . "\t\t" . nr . " " . a:toc[openfile][line][2])
		    endif
		else
		    let nr=secnr 
		    if a:toc[openfile][line][3]
			"if it is stared version
			let nr=substitute(nr,'.',' ','g')
		    endif
		    if a:toc[openfile][line][4] != ''
			call setline (number, showline . "\t" . nr . " " . a:toc[openfile][line][4])
		    else
			call setline (number, showline . "\t" . nr . " " . a:toc[openfile][line][2])
		    endif
		endif
	    elseif a:toc[openfile][line][0] == 'subsection'
		let ssecnr=a:toc[openfile][line][1]
		if chap_on
		    let nr=chnr . "." . secnr  . "." . ssecnr
		    if a:toc[openfile][line][3]
			"if it is stared version 
			let nr=substitute(nr,'.',' ','g')
		    endif
		    if a:toc[openfile][line][4] != ''
			call setline (number, showline . "\t\t\t" . nr . " " . a:toc[openfile][line][4])
		    else
			call setline (number, showline . "\t\t\t" . nr . " " . a:toc[openfile][line][2])
		    endif
		else
		    let nr=secnr  . "." . ssecnr
		    if a:toc[openfile][line][3]
			"if it is stared version 
			let nr=substitute(nr,'.',' ','g')
		    endif
		    if a:toc[openfile][line][4] != ''
			call setline (number, showline . "\t\t" . nr . " " . a:toc[openfile][line][4])
		    else
			call setline (number, showline . "\t\t" . nr . " " . a:toc[openfile][line][2])
		    endif
		endif
	    elseif a:toc[openfile][line][0] == 'subsubsection'
		let sssecnr=a:toc[openfile][line][1]
		if chap_on
		    let nr=chnr . "." . secnr . "." . sssecnr  
		    if a:toc[openfile][line][3]
			"if it is stared version
			let nr=substitute(nr,'.',' ','g')
		    endif
		    if a:toc[openfile][line][4] != ''
			call setline(number, a:toc[openfile][line][0] . "\t\t\t" . nr . " " . a:toc[openfile][line][4])
		    else
			call setline(number, a:toc[openfile][line][0] . "\t\t\t" . nr . " " . a:toc[openfile][line][2])
		    endif
		else
		    let nr=secnr  . "." . ssecnr . "." . sssecnr
		    if a:toc[openfile][line][3]
			"if it is stared version 
			let nr=substitute(nr,'.',' ','g')
		    endif
		    if a:toc[openfile][line][4] != ''
			call setline (number, showline . "\t\t" . nr . " " . a:toc[openfile][line][4])
		    else
			call setline (number, showline . "\t\t" . nr . " " . a:toc[openfile][line][2])
		    endif
		endif
	    else
		let nr=""
	    endif
	    let number+=1
	endfor
    endfor
    " set the cursor position on the correct line number.
    " first get the line number of the begging of the ToC of t:atp_bufname
    " (current buffer)
" 	let t:numberdict=numberdict	"DEBUG
" 	t:atp_bufname is the full path to the current buffer.
    let num = get(numberdict, t:atp_bufname, 'no_number')
    if num == 'no_number'
" 	call s:TOC("")
	return
    endif
    let sorted		= sort(keys(a:toc[t:atp_bufname]), "atplib#CompareNumbers")
    let t:sorted	= sorted
    for line in sorted
	if cline>=line
	    let num+=1
	endif
    keepjumps call setpos('.',[bufnr(""),num,1,0])
    endfor
   
    " Help Lines:
    if search('<Enter> jump and close', 'nW') == 0
	call append('$', [ '', 			
		\ '<Space> jump', 
		\ '<Enter> jump and close', 	
		\ 's       jump and split', 
		\ 'y or c  yank label', 	
		\ 'p       paste label', 
		\ 'q       close', 		
		\ ':YankSection', 
		\ ':DeleteSection', 
		\ ':PasteSection', 		
		\ ':SectionStack', 
		\ ':Undo' ])
    endif
    lockvar 3 b:atp_Toc
endfunction
" {{{3 ToCbufnr()
" This function returns toc buffer number if toc window is not open returns -1.
function! <SID>ToCbufnr() 
    return index(map(tabpagebuflist(), 'bufname(v:val)'), '__ToC__')
endfunction
" {{{3 UpdateToCLine
function! UpdateToCLine(...)
    if !g:atp_UpdateToCLine
	return
    endif
    let toc_bufnr	= <SID>ToCbufnr()
    let check_line 	= (a:0>=1 ? a:1 : -1) 
    if toc_bufnr == -1 || check_line != -1 && 
		\ getline(line(".")+check_line) !~# '\\\%(part\|chapter\|\%(sub\)\{0,2}section\)\s*{'
	return
    endif
    let cline  	= line(".")
    let cbufnr 	= bufnr("")
    let cwinnr	= bufwinnr("")
    exe toc_bufnr."wincmd w"
    let num 	= get(s:numberdict, t:atp_bufname, 'no_number')
    if num == 'no_number'
	exe cwinnr."wincmd w"
	return
    endif
    let sorted	= sort(keys(t:atp_toc[t:atp_bufname]), "atplib#CompareNumbers")
    for line in sorted
	if cline>=line
	    let num+=1
	endif
	keepjumps call setpos('.',[bufnr(""),num,1,0])
	call atplib#CursorLine()
    endfor

    let eventignore=&eventignore
    set eventignore+=BufEnter
    exe cwinnr."wincmd w"
    let &eventignore=eventignore
endfunction
" This is User Front End Function 
"{{{3 TOC
function! <SID>TOC(bang,...)
    " skip generating t:atp_toc list if it exists and if a:0 != 0
    if &l:filetype != 'tex' && &l:filetype != 'toc_atp'   
	echoerr "Wrong 'filetype'. This command works only for latex documents."
	return
    endif
    if a:0 == 0 
	call s:buflist()
    endif
    " for each buffer in t:atp_toc_buflist (set by s:buflist)
    if ( a:bang == "!" || !exists("t:atp_toc") )
	let t:atp_toc = {}
	for buffer in t:atp_toc_buflist 
" 	    let t:atp_toc=s:maketoc(buffer)
	    call extend(t:atp_toc, s:maketoc(buffer))
	endfor
    endif
    call s:showtoc(t:atp_toc)
endfunction
nnoremap <Plug>ATP_TOC			:call <SID>TOC("")<CR>

" This finds the name of currently eddited section/chapter units. 
" {{{3 Current TOC
" ToDo: make this faster!
" {{{3 s:NearestSection
" This function finds the section name of the current section unit with
" respect to the dictionary a:section={ 'line number' : 'section name', ... }
" it returns the [ section_name, section line, next section line ]
function! <SID>NearestSection(section)
    let cline=line('.')

    let sorted=sort(keys(a:section), "atplib#CompareNumbers")
    let x=0
    while x<len(sorted) && sorted[x]<=cline
       let x+=1 
    endwhile
    if x>=1 && x < len(sorted)
	let section_name=a:section[sorted[x-1]]
	return [section_name, sorted[x-1], sorted[x]]
    elseif x>=1 && x >= len(sorted)
	let section_name=a:section[sorted[x-1]]
	return [section_name,sorted[x-1], line('$')]
    elseif x<1 && x < len(sorted)
	" if we are before the first section return the empty string
	return ['','0', sorted[x]]
    elseif x<1 && x >= len(sorted)
	return ['', '0', line('$')]
    endif
endfunction
" {{{3 s:ctoc
function! <SID>ctoc()
    if &l:filetype != 'tex' 
" TO DO:
" 	if  exists(g:tex_flavor)
" 	    if g:tex_flavor != "latex"
" 		echomsg "CTOC: Wrong 'filetype'. This function works only for latex documents."
" 	    endif
" 	endif
	" Set the status line once more, to remove the CTOC() function.
	call ATPStatus()
	return []
    endif
    " resolve the full path:
    let t:atp_bufname=resolve(fnamemodify(bufname("%"),":p"))
    
    " if t:atp_toc(t:atp_bufname) exists use it otherwise make it 
    if !exists("t:atp_toc") || !has_key(t:atp_toc, t:atp_bufname) 
	if !exists("t:atp_toc")
	    let t:atp_toc={}
	endif
	silent call extend(t:atp_toc, s:maketoc(t:atp_bufname))
    endif

    " l:count where the preambule ends
    let buffer=getbufline(bufname("%"),"1","$")
    let i=0
    let line=buffer[0]
    while line !~ '\\begin\s*{document}' && i < len(buffer)
	let line=buffer[i]
	if line !~ '\\begin\s*{document}' 
	    let i+=1
	endif
    endwhile
	
    " if we are before the '\\begin{document}' line: 
    if line(".") <= i
	let return=['Preambule']
	return return
    endif

    let chapter={}
    let section={}
    let subsection={}

    for key in keys(t:atp_toc[t:atp_bufname])
	if t:atp_toc[t:atp_bufname][key][0] == 'chapter'
	    " return the short title if it is provided
	    if t:atp_toc[t:atp_bufname][key][4] != ''
		call extend(chapter, {key : t:atp_toc[t:atp_bufname][key][4]},'force')
	    else
		call extend(chapter, {key : t:atp_toc[t:atp_bufname][key][2]},'force')
	    endif
	elseif t:atp_toc[t:atp_bufname][key][0] == 'section'
	    " return the short title if it is provided
	    if t:atp_toc[t:atp_bufname][key][4] != ''
		call extend(section, {key : t:atp_toc[t:atp_bufname][key][4]},'force')
	    else
		call extend(section, {key : t:atp_toc[t:atp_bufname][key][2]},'force')
	    endif
	elseif t:atp_toc[t:atp_bufname][key][0] == 'subsection'
	    " return the short title if it is provided
	    if t:atp_toc[t:atp_bufname][key][4] != ''
		call extend(subsection, {key : t:atp_toc[t:atp_bufname][key][4]},'force')
	    else
		call extend(subsection, {key : t:atp_toc[t:atp_bufname][key][2]},'force')
	    endif
	endif
    endfor

    " Remove $ from chapter/section/subsection names to save the space.
    let chapter_name=substitute(s:NearestSection(chapter)[0],'\$\|\\(\|\\)','','g')
    let chapter_line=s:NearestSection(chapter)[1]
    let chapter_nline=s:NearestSection(chapter)[2]

    let section_name=substitute(s:NearestSection(section)[0],'\$\|\\(\|\\)','','g')
    let section_line=s:NearestSection(section)[1]
    let section_nline=s:NearestSection(section)[2]
"     let b:section=s:NearestSection(section)		" DEBUG

    let subsection_name=substitute(s:NearestSection(subsection)[0],'\$\|\\(\|\\)','','g')
    let subsection_line=s:NearestSection(subsection)[1]
    let subsection_nline=s:NearestSection(subsection)[2]
"     let b:ssection=s:NearestSection(subsection)		" DEBUG

    let names	= [ chapter_name ]
    if (section_line+0 >= chapter_line+0 && section_line+0 <= chapter_nline+0) || chapter_name == '' 
	call add(names, section_name) 
    elseif subsection_line+0 >= section_line+0 && subsection_line+0 <= section_nline+0
	call add(names, subsection_name)
    endif
    return names
endfunction
" {{{3 CTOC
function! CTOC(...)
    " if there is any argument given, then the function returns the value
    " (used by ATPStatus()), otherwise it echoes the section/subsection
    " title. It returns only the first b:atp_TruncateStatusSection
    " characters of the the whole titles.
    let names=s:ctoc()
    let g:names=names
    let chapter_name	= get(names, 0, '')
    let section_name	= get(names, 1, '')
    let subsection_name	= get(names, 2, '')

    if chapter_name == "" && section_name == "" && subsection_name == ""

    if a:0 == '0'
	echo "" 
    else
	return ""
    endif
	
    elseif chapter_name != ""
	if section_name != ""
	    if a:0 != 0
		return substitute(strpart(chapter_name,0,b:atp_TruncateStatusSection/2), '\_s*$', '','') . "/" . substitute(strpart(section_name,0,b:atp_TruncateStatusSection/2), '\_s*$', '','')
	    endif
	else
	    if a:0 != 0
		return substitute(strpart(chapter_name,0,b:atp_TruncateStatusSection), '\_s*$', '','')
	    endif
	endif
    elseif chapter_name == "" && section_name != ""
	if subsection_name != ""
	    if a:0 != 0
		return substitute(strpart(section_name,0,b:atp_TruncateStatusSection/2), '\_s*$', '','') . "/" . substitute(strpart(subsection_name,0,b:atp_TruncateStatusSection/2), '\_s*$', '','')
	    endif
	else
	    if a:0 != 0
		return substitute(strpart(section_name,0,b:atp_TruncateStatusSection), '\_s*$', '','')
	    endif
	endif
    elseif chapter_name == "" && section_name == "" && subsection_name != ""
	if a:0 != 0
	    return substitute(strpart(subsection_name,0,b:atp_TruncateStatusSection), '\_s*$', '','')
	endif
    endif
endfunction "}}}3

" Labels Front End Finction. The search engine/show function are in autoload/atplib.vim script
" library.
" {{{2 Labels
" a:bang = "!" do not regenerate labels if not necessary
function! <SID>Labels(bang)
    let t:atp_bufname	= bufname("%")
    let error		= ( exists("b:atp_TexReturnCode") ? b:atp_TexReturnCode : 0 )
    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)

    " Generate the dictionary with labels
    if a:bang == "" || ( a:bang == "!" && !exists("t:atp_labels") )
	let [ t:atp_labels, b:ListOfFiles ] =  atplib#generatelabels(atp_MainFile, 1)
    endif

    " Show the labels in seprate window
    call atplib#showlabels([ t:atp_labels, map(extend([b:atp_MainFile], copy(b:ListOfFiles)), 'atplib#FullPath(v:val)')])

    if error
	echohl WarningMsg
	redraw
	echomsg "[ATP:] the compelation contains errors, aux file might be not appriopriate for labels window."
	echohl Normal
    endif
endfunction
nnoremap <Plug>ATP_Labels		:call <SID>Labels("")<CR>

" GotoLabel & GotoLabelCompletion {{{2
" a:bang = "!" do not regenerate labels if not necessary
" This is developed for one tex project in a vim.
function! GotoLabel(bang,...)

    let alabel = ( a:0 == 0 ? "" : a:1 )

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    " Generate the dictionary with labels
    if a:bang == "" || ( a:bang == "!" && ( !exists("b:ListOfFiles") || !exists("t:atp_labels") ) )
	let [ t:atp_labels, b:ListOfFiles ] =  atplib#generatelabels(atp_MainFile, 1)
    endif

    let matches = []
    for file in keys(t:atp_labels)
	if index(b:ListOfFiles, fnamemodify(file, ":t")) != -1 || index(b:ListOfFiles, file) != -1 || file == atplib#FullPath(b:atp_MainFile)
	    for label in t:atp_labels[file]
		if label[1] =~ alabel || label[2] =~ '^'.alabel
		    call add(matches, extend([file], label))
		endif
	    endfor
	endif
    endfor

    if len(matches) == 0
	redraw
	echohl WarningMsg
	echomsg "[ATP:] no matching label"
	echohl Normal
	return 1
    elseif len(matches) == 1
	let file=matches[0][0]
	let line=matches[0][1]
    else
" 	if len(keys(filter(copy(b:TypeDict), 'v:val == "input"'))) == 0
	    let mlabels=map(copy(matches), "[(index(matches, v:val)+1).'.', v:val[2],v:val[3]]")
" 	else
" 	Show File from which label comes
" 	The reason to not use this is as follows: 
" 		it only matters for project files, which probably have many
" 		labels, so it's better to make the list as concise as possible
" 	    let mlabels=map(copy(matches), "[(index(matches, v:val)+1).'.', v:val[2], v:val[3], fnamemodify(v:val[0], ':t')]")
" 	    let file=1 
" 	endif
	echohl Title
	echo "Which label to choose?"
	echohl Normal
" 	let mlabels= ( file ? extend([[' nr', 'LABEL', 'LABEL NR', 'FILE']], mlabels) : extend([[' nr', 'LABEL', 'LABEL NR']], mlabels) )
	let g:mlabels=copy(mlabels)
	for row in atplib#FormatListinColumns(atplib#Table(mlabels, [1,2]),2)
	    echo join(row)
	endfor
	let nr = input("Which label to choose? type number and press <Enter> ")-1
	if nr < 0 || nr >= len(matches)
	    return
	endif
	let file=matches[nr][0]
	let line=matches[nr][1]
    endif

    " Check if the buffer is loaded.
    if bufloaded(file)
	execute "b " . file
	call cursor(line,1)
    else
	execute "edit " . file
	call cursor(line,1)
    endif
endfunction

function! GotoLabelCompletion(ArgLead, CmdLine, CursorPos)

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    " Generate the dictionary with labels (only if it doesn't exist)
    if !exists("t:atp_labels") || t:atp_labels == {} || !exists("b:ListOfFiles") || a:CmdLine !~# '^GotoLabel!'
	let [ t:atp_labels, b:ListOfFiles ] =  atplib#generatelabels(atp_MainFile, 1)
" It would be nice to delete the ! from the cmdline after this step. There are
" only getcmdline(), getcmdpos() and setcmdpos() functions available.
	let cmd_line=substitute(getcmdline(), "GotoLabel!", "GotoLabel", "")
    endif

    let labels=[]
    for file in keys(t:atp_labels)
	if index(b:ListOfFiles, fnamemodify(file, ":t")) != -1 || index(b:ListOfFiles, file) != -1 || file == atplib#FullPath(b:atp_MainFile)
	    call extend(labels, map(deepcopy(t:atp_labels)[file], 'v:val[1]'))
	    call extend(labels, map(deepcopy(t:atp_labels)[file], 'v:val[2]'))
	endif
    endfor
    let g:labels=copy(labels)
    call filter(labels, "v:val !~ '^\s*$' && v:val =~ a:ArgLead ")

    return map(labels, "v:val.'\\>'")
endfunction
" {{{2 TAGS
function! <SID>LatexTags(bang)
    let hyperref_cmd = ( atplib#SearchPackage("hyperref") ? " --hyperref " : "" )
    if has("clientserver")
	let servername 	= " --servername ".v:servername." "
	let progname	= " --progname ".v:progname." " 
    else
	let servername 	= ""
	let progname	= ""
    endif
    let bibtags = ( a:bang == "" ? " --bibtags " : "" )
    " Write file (disable project file):
    let project=b:atp_ProjectScript
    let b:atp_ProjectScript=0
    silent! write
    let b:atp_ProjectScript=project

    let latextags=globpath(&rtp, "ftplugin/ATP_files/latextags.py")
    let files=join(
		\ map([b:atp_MainFile]+filter(copy(keys(b:TypeDict)), "b:TypeDict[v:val] == 'input'"),
		    \ 'atplib#FullPath(v:val)')
		\ , ";")
    
    if len(filter(copy(keys(b:TypeDict)), "b:TypeDict[v:val] == 'bib'")) >= 1
	let bibfiles=join(filter(copy(keys(b:TypeDict)), "b:TypeDict[v:val] == 'bib'"), ";")
	let bib= " --bibfiles ".shellescape(bibfiles) 
    else
	let bib= " --bibtags_env "
    endif
    let dir 	= expand("%:p:h")
    if atplib#SearchPackage("biblatex")
	let cite = " --cite biblatex "
    elseif atplib#SearchPackage("natbib")
	let cite = " --cite natbib "
    else
	let cite = " "
    endif

    let cmd=g:atp_Python." ".shellescape(latextags).
		\ " --files ".shellescape(files).
		\ " --auxfile ".shellescape(fnamemodify(atplib#FullPath(b:atp_MainFile), ":r").".aux").
		\ " --dir ".shellescape(dir).
		\ bib . cite .
		\ hyperref_cmd . servername . progname . bibtags . " &"
    if g:atp_debugLatexTags
	let g:cmd=cmd
    endif
    call system(cmd)
endfunction
"{{{2 GotoDestination
function! <SID>GotoNamedDestination(destination)
    if b:atp_Viewer !~ '^\s*xpdf\>' 
	echomsg "[ATP:] this only works with Xpdf viewer."
	return 0
    endif
    let cmd='xpdf -remote '.b:atp_XpdfServer.' -exec gotoDest\("'.a:destination.'"\)'
    call system(cmd)
endfunction
function! <SID>FindDestinations()
    let files = [ b:atp_MainFile ]
    if !exists("b:TypeDict")
	call TreeOfFiles(b:atp_MainFile)
    endif
    for file in keys(b:TypeDict)
	if b:TypeDict[file] == 'input'
	    call add(files, file)
	endif
    endfor
    let saved_loclist = getloclist(0)
    exe 'lvimgrep /\\hypertarget\>/gj ' . join(map(files, 'fnameescape(v:val)'), ' ') 
    let dests = []
    let loclist	= copy(getloclist(0))
    let g:loclist = loclist
    call setloclist(0, saved_loclist)
    for loc in loclist
	let destname = matchstr(loc['text'], '\\hypertarget\s*{\s*\zs[^}]*\ze}')
	call add(dests, destname)
    endfor
    return dests
endfunction
function! <SID>CompleteDestinations(ArgLead, CmdLine, CursorPos)
    let dests=<SID>FindDestinations()
    return join(dests, "\n")
endfunction

" Motion functions through environments and sections. 
" {{{2 Motion functions
" Go to next environment "{{{3
" which name is given as the argument. Do not wrap
" around the end of the file.
function! <SID>GotoEnvironment(flag,count,...)

    " Options :
    let env_name 	= ( a:0 >= 1 && a:1 != ""  ? a:1 : '[^}]*' )
    if env_name == 'part'
	if a:flag =~ 'b'
	    exe a:count.'PPart'
	    return
	else
	    exe a:count.'NPart'
	    return
	endif
    elseif env_name  == 'chapter' 
	if a:flag =~ 'b'
	    exe a:count.'PChap'
	    return
	else
	    exe a:count.'NChap'
	    return
	endif
    elseif env_name == 'section' 
	if a:flag =~ 'b'
	    exe a:count.'PSec'
	    return
	else
	    exe a:count.'NSec'
	    return
	endif
    elseif env_name == 'subsection' 
	if a:flag =~ 'b'
	    exe a:count.'PSSec'
	    return
	else
	    exe a:count.'NSSec'
	    return
	endif
    elseif env_name == 'subsubsection' 
	if a:flag =~ 'b'
	    exe a:count.'PSSSec'
	    return
	else
	    exe a:count.'NSSSec'
	    return
	endif
    endif

    let flag = a:flag
    
    " Set the search tool :
    " Set the pattern : 
    if env_name == 'math'
	let pattern = '\m\%(\(\\\@<!\\\)\@<!%.*\)\@<!\%(\%(\\begin\s*{\s*\%(\(displayed\)\?math\|\%(fl\)\?align\|eqnarray\|equation\|gather\|multline\|subequations\|xalignat\|xxalignat\)\s*\*\=\s*}\)\|\\\@<!\\\[\|\\\@<!\\(\|\\\@<!\$\$\=\)'
    elseif env_name == 'displayedmath'
	let pattern = '\m\%(\(\\\@<!\\\)\@<!%.*\)\@<!\%(\%(\\begin\s*{\s*\%(displayedmath\|\%(fl\)\?align\*\=\|eqnarray\*\=\|equation\*\=\|gather\*\=\|multline\*\=\|xalignat\*\=\|xxalignat\*\=\)\s*}\)\|\\\@<!\\\[\|\\\@!\$\$\)'
    elseif env_name == 'inlinemath'
	let pattern = '\m\%(\(\\\@<!\\\)\@<!%.*\)\@<!\%(\\begin\s*{\s*math\s*}\|\\\@<!\\(\|\$\@<!\\\@<!\$\$\@!\)'
    else
	let pattern = '\m\%(\(\\\@<!\\\)\@<!%.*\)\@<!\\begin\s*{\s*' . env_name 
    endif


    " Search (twise if needed)
    for i in range(1, a:count)
	if i > 1
	    " the 's' flag should be used only in the first search. 
	    let flag=substitute(flag, 's', '', 'g') 
	endif
	if g:atp_mapNn
	    let search_cmd 	= "S /"
	    let search_cmd_e= "/ " . flag
	else
	    let search_cmd	= "silent! call search('"
	    let search_cmd_e= "','" . flag . "')"
	endif
	execute  search_cmd . pattern . search_cmd_e
	if a:flag !~# 'b'
	    if getline(".")[col(".")-1] == "$" 
		if ( get(split(getline("."), '\zs'), col(".")-1, '') == "$" && get(split(getline("."), '\zs'), col("."), '') == "$" )
		    "check $$
		    let rerun = !atplib#CheckSyntaxGroups(['texMathZoneY'], line("."), col(".")+1 )
		elseif get(split(getline("."), '\zs'), col(".")-1, '') == "$" 
		    "check $
		    let rerun = !atplib#CheckSyntaxGroups(['texMathZoneX', 'texMathZoneY'], line("."), col(".") )
		endif
		if rerun
		    silent! execute search_cmd . pattern . search_cmd_e
		endif
	    endif
	else " a:flag =~# 'b'
	    if getline(".")[col(".")-1] == "$" 
		if ( get(split(getline("."), '\zs'), col(".")-1, '') == "$" && get(split(getline("."), '\zs'), col(".")-2, '') == "$" )
		    "check $$
		    let rerun = atplib#CheckSyntaxGroups(['texMathZoneY'], line("."), col(".")-3 )
		elseif get(split(getline("."), '\zs'), col(".")-1, '') == "$" 
		    "check $
		    let rerun = atplib#CheckSyntaxGroups(['texMathZoneX', 'texMathZoneY'], line("."), col(".")-2 )
		endif
		if rerun
		    silent! execute search_cmd . pattern . search_cmd_e
		endif
	    endif
	endif
    endfor

    call UpdateToCLine()
    silent! call histadd("search", pattern)
    silent! let @/ 	 = pattern
    return ""
endfunction
" function! <SID>GotoEnvironmentB(flag,count,...)
"     let env_name 	= (a:0 >= 1 && a:1 != ""  ? a:1 : '[^}]*')
"     for i in range(1,a:count)
" 	let flag 	= (i!=1?substitute(a:flag, 's', '', 'g'):a:flag)
" 	call <SID>GotoEnvironment(flag,1,env_name)
"     endfor
" endfunction
" Jump over current \begin and go to next one. {{{3
" i.e. if on line =~ \begin => % and then search, else search
function! <SID>JumptoEnvironment(backward)
    call setpos("''", getpos("."))
    let lazyredraw=&l:lazyredraw
    set lazyredraw
    if !a:backward
	let col	= searchpos('\w*\>\zs', 'n')[1]-1
	if strpart(getline(line(".")), 0, col) =~ '\\begin\>$' &&
		    \ strpart(getline(line(".")), col) !~ '^\s*{\s*document\s*}'
	    exe "normal %"
	endif
	call search('^\%([^%]\|\\%\)*\zs\\begin\>', 'W')
    else
	let found =  search('^\%([^%]\|\\%\)*\\end\>', 'bcW')
	if getline(line(".")) !~ '^\%([^%]\|\\%\)*\\end\s*{\s*document\s*}' && found
	    exe "normal %"
	elseif !found
	    call search('^\%([^%]\|\\%\)*\zs\\begin\>', 'bW')
	endif
    endif
    let &l:lazyredraw=lazyredraw
endfunction 
" Go to next section {{{3 
" The extra argument is a pattern to match for the
" section title. The first, obsolete argument stands for:
" part,chapter,section,subsection,etc.
" This commands wrap around the end of the file.
" with a:3 = 'vim' it uses vim search() function
" with a:3 = 'atp' 
" the default is: 
" 	if g:atp_mapNn then use 'atp'
" 	else use 'vim'.
function! <SID>GotoSection(bang, count, flag, secname, ...)
    let search_tool		= ( a:0 >= 1 ? a:1	: ( g:atp_mapNn ? 'atp' : 'vim' ) )
    let mode			= ( a:0 >= 2 ? a:2	: 'n' )
    let title_pattern 		= ( a:0 >= 3 ? a:3	: ''  )
    let pattern = ( empty(a:bang) ? '^\([^%]\|\\\@<!\\%\)*' . a:secname . title_pattern : a:secname . title_pattern )

    if getline(line(".")) =~ pattern
	" If we are on the line that matches go to begining of this line, so
	" that search will find previous match unless the flag contains 'c'.
	call cursor(line("."), 1)
    endif

    " This is not working ?:/
    " just because it goes back to the mark '< and searches again:
"     if mode == 'v' | call cursor(getpos("'<")[1], getpos("'<")[2]) | endif
"     if mode == 'v' && visualmode() ==# 'V'
" 	normal! V
"     elseif mode == 'v' 
" 	normal! v
"     endif
"     let bpat = ( mode == 'v' 	? "\\n\\s*" : "" ) 
    let bpat 	= "" 
    let flag	= a:flag
    for i in range(1,a:count)
	if i > 1
	    " the 's' flag should be used only in the first search. 
	    let flag=substitute(flag, 's', '', 'g') 
	endif
	if search_tool == 'vim'
	    call searchpos(bpat . pattern, flag)
	else
	    execute "S /". bpat . pattern . "/ " . flag 
	endif
    endfor

    call UpdateToCLine()
    call histadd("search", pattern)
    let @/	= pattern
endfunction
function! Env_compl(A,P,L) 
    let envlist=sort(['algorithm', 'algorithmic', 'abstract', 'definition', 'equation', 'proposition', 
		\ 'theorem', 'lemma', 'array', 'tikzpicture', 
		\ 'tabular', 'table', 'align', 'alignat', 'proof', 
		\ 'corollary', 'enumerate', 'examples\=', 'itemize', 'remark', 
		\ 'notation', 'center', 'quotation', 'quote', 'tabbing', 
		\ 'picture', 'math', 'displaymath', 'minipage', 'list', 'flushright', 'flushleft', 
		\ 'frame', 'figure', 'eqnarray', 'thebibliography', 'titlepage', 
		\ 'verbatim', 'verse', 'inlinemath', 'displayedmath', 'subequations',
		\ 'part', 'section', 'subsection', 'subsubsection' ])
    let returnlist=[]
    for env in envlist
	if env =~ '^' . a:A 
	    call add(returnlist,env)
	endif
    endfor
    return returnlist
endfunction

function! <SID>ggGotoSection(count,section)
    let mark  = getpos("''")
    if a:section == "part"
	let secname = '\\part\>'
	call cursor(1,1)
    elseif a:section == "chapter"
	let secname = '\\\%(part\|chapter\)\>'
	if !search('\\part\>', 'bc')
	    call cursor(1,1)
	endif
    elseif a:section == "section"
	let secname = '\\\%(part\|chapter\|section\)\>'
	if !search('\\chapter\>\|\\part\>', 'bc')
	    call cursor(1,1)
	endif
    elseif a:section == "subsection"
	let secname = '\\\%(part\|chapter\|section\|subsection\)\>'
	if !search('\\section\>\|\\chapter\>\|\\part\>', 'bc')
	    call cursor(1,1)
	endif
    elseif a:section == "subsubsection"
	let secname = '\\\%(part\|chapter\|section\|subsection\|subsubsection\)\>'
	if !search('\subsection\>\|\\section\>\|\\chapter\>\|\\part\>', 'bc')
	    call cursor(1,1)
	endif
    endif
    call UpdateToCLine()
    call <SID>GotoSection("", a:count, 'Ws', secname)
    call setpos("''",mark)
endfunction

" {{{3 Input() function
function! <SID>Input(flag)
    let pat 	= ( &l:filetype == "plaintex" ? '\\input\s*{' : '\%(\\input\>\|\\include\s*{\)' )
    let @/	= '^\([^%]\|\\\@<!\\%\)*' . pat
    if g:atp_mapNn
	exe ':S /^\([^%]\|\\\@<!\\%\)*' .  pat . '/ ' . a:flag
    else
	call search('^\([^%]\|\\\@<!\\%\)*' . pat, a:flag)
    endif
    call UpdateToCLine()
    "     This pattern is quite simple and it might be not neccesary to add it to
    "     search history.
    "     call histadd("search", pat)
endfunction
" {{{2 Go to File
" This function also sets filetype vim option.
" It is useing '\f' pattern thus it depends on the 'isfname' vim option.
try
    " NOTE: if the filetype is wrong the path will not be recognized
    " 		it is better to make it syntax independet!
    "
    " It let choose if there are multiple files only when this is fast
    " (\input{,\input ) methods. However, then the file name should be unique! 

    " It correctly sets b:atp_MainFile, and TreeOfFiles, ... variables in the new
    " buffer.
function! GotoFile(bang,file,...)

    let cwd	= getcwd()
    exe "lcd " . fnameescape(b:atp_ProjectDir)
    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)

    " The default value is to check line if not stending on a comment.
    let check_line	= ( a:0 >= 1 ? a:1 : strpart(getline("."), 0, col(".")) !~ '\(\\\@<!\\\)\@<!%' )

    if !has("path_extra")
	echoerr "Needs +path_extra vim feature."
	exe "lcs " . cwd
	return
    endif	

    let filetype 	= &l:filetype

    if a:bang == "!" || !exists("b:TreeOfFiles") || !exists("b:ListOfFiles") || !exists("b:TypeDict") || !exists("b:LevelDict") 
	let [tree_d, file_l, type_d, level_d ] 	= TreeOfFiles(atp_MainFile)
    else
	let [tree_d, file_l, type_d, level_d ] 	= deepcopy([ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ])
    endif

    " This is passed to the newly opened buffer.
    let projectVarDict = SaveProjectVariables()

    let file_l_orig = deepcopy(file_l)

    " Note: line is set to "" if check_line == 0 => method = "all" is used. 
    let line		= ( check_line ? getline(".") : "" )
    if check_line
	let beg_line	= strpart(line, 0,col(".")-1)
	" Find the begining columnt of the file name:
	let bcol	= searchpos('\%({\|,\)', 'bn',  line("."))[1]
	if bcol == 0
	    let bcol 	= searchpos('{', 'n', line("."))[1]
	endif

	" Find the end column of the file name
	let col		= searchpos(',\|}', 'cn', line("."))[1]
	" Current column
	let cur_col		= col(".")
    endif

    " This part will be omitted if check_line is 0 (see note above).
    " \usepackege{...,<package_name>,...}
    if line =~ '\\usepackage' && g:atp_developer
	let method = "usepackage"
	    let ext 	= '.sty'

	    let fname   = atplib#append_ext(strpart(getline("."), bcol, col-bcol-1), ext)
	    let file 	= atplib#KpsewhichFindFile('tex', fname, g:atp_texinputs, 1)
	    let file_l	= [ file ]

	    let message = "Pacakge: "
	    let options = ""

    " \input{...}, \include{...}
    elseif line =~ '\\\(input\|include\)\s*{' 
	let method = "input{"
	    let ext 	= '.tex'

	    " \input{} doesn't allow for {...,...} many file path. 
	    let fname 	= atplib#append_ext(strpart(getline("."), bcol, col-bcol-1), '.tex')

	    " The 'file . ext' might be already a full path.
	    if fnamemodify(fname, ":p") != fname
		let file_l 	= atplib#KpsewhichFindFile('tex', fname, g:atp_texinputs, -1, ':p', '^\(\/home\|\.\)', '\%(^\/usr\|kpsewhich\|texlive\|miktex\)')
		let file	= get(file_l, 0, 'file_missing')
	    else
		let file_l	= [ fname ] 
		let file	= fname
	    endif

	    let message = "File: "
	    let options = ""

    " \input 	/without {/
    elseif line =~ '\\input\s*{\@!'
	let method = "input"
	    let fname	= atplib#append_ext(matchstr(getline(line(".")), '\\input\s*\zs\f*\ze'), '.tex')
	    let file_l	= atplib#KpsewhichFindFile('tex', fname, g:atp_texinputs, -1, ':p', '^\(\/home\|\.\)', '\%(^\/usr\|kpsewhich\|texlive\)')
	    let file	= get(file_l, 0, "file_missing")
	    let options = ' +setl\ ft=' . &l:filetype  

    " \documentclass{...}
    elseif line =~ '\\documentclass' && g:atp_developer
	let method = "documentclass"
	let saved_pos	= getpos(".")
	call cursor(line("."), 1)
	call search('\\documentclass\zs', 'cb', line("."))
	let bcol	= searchpos('{', 'c', line("."))[1]
	execute "normal %"
	let ecol	= col(".")
	call cursor(saved_pos[0], saved_pos[1])
	let classname 	= strpart(getline("."), bcol, ecol-bcol-1)

	let fname	= atplib#append_ext(classname, '.cls')
	let file	= atplib#KpsewhichFindFile('tex', fname,  g:atp_texinputs, ':p')
	let file_l	= [ file ]
	let options	= ""
    else
	" If not over any above give a list of input files to open, like
	" EditInputFile  
	let method	= "all"

	call extend(file_l, [ atp_MainFile ], 0)
	call extend(level_d, { atp_MainFile : 0 })
    endif

    if len(file_l) > 1 && a:file =~ '^\s*$'
	if method == "all"
	    let msg = "Which file to edit?"
	else
	    let msg = "Found many files. Which file to use?"
	endif
	let mods	= method == 'all' ? ":t" : ":p"
	" It is better to start numbering from 0,
	" then 	0 - is the main file 
	"	1 - is the first chapter, and so on.
	let i		= 0
	let input_l	= []
	for f in file_l
	    if exists("level_d")
		let space = ""
		if g:atp_RelativePath
		    let cwd = getcwd()
		    exe "lcd " . fnameescape(b:atp_ProjectDir)
		    let level = get(level_d,fnamemodify(f, ':.'), get(level_d, f, 1))
		    exe "lcd " . fnameescape(cwd)
		else
		    let cwd = getcwd()
		    exe "lcd " . fnameescape(b:atp_ProjectDir)
		    let level = get(level_d,f, get(level_d,fnamemodify(f, ':.'), 1))
		    exe "lcd " . fnameescape(cwd)
		endif
		for j in range(level)
		    let space .= "   "
		endfor
	    else
		space	= ""
	    endif
	    call add(input_l, "(" . i . ") " . space . fnamemodify(f, mods))
	    let i+=1
	endfor
	" Ask the user which file to edit:
	redraw
	if len([ msg ] + input_l) < &l:lines
	    for f in  [ msg ] + input_l
		" echo highlighted message
		if matchstr(f, '(\d\+)\s*\zs.*$') == expand("%:t")
		    echohl CursorLine
		elseif f == msg
		    echohl Title
		endif
		echo f
		if matchstr(f, '(\d\+)\s*\zs.*$') == expand("%:t") || f == msg
		    echohl Normal
		endif
	    endfor
	    let choice	= input("Type number and <Enter> (empty cancels): ")
	    if choice != "" 
		let choice	+= 1
	    endif
	elseif 
	    for line in [ msg ] + input_l
		if line == msg
		    echohl Title	
		endif
		echo line
		echohl None
	    endfor
	    echohl MoreMsg
	    let choice = input("Type number and <Enter> (empty cancels): ")
	    echohl None
	    if choice != "" 
		let choice	+= 1
	    endif
	endif
	" Remember: 0 == "" returns 1! 
	" char2nr("") = 0
	" nr2char(0) = ""
	if choice == ""
	    exe "lcd " . fnameescape(cwd)
	    return
	endif
	if choice < 1 || choice > len(file_l)
	    if choice < 1 || choice > len(file_l)
		echo "\n"
		echoerr "Choice out of range."
	    endif
	    exe "lcd " . fnameescape(cwd)
	    return
	endif
	let file 	= file_l[choice-1]
	let fname 	= file
    elseif a:file !~ '^\s*$'
	let file 	= atplib#FullPath(a:file)
	let fname	= file
    endif

"     DEBUG
"     let g:fname  = fname
"     let g:file   = file 
"     let g:file_l = file_l
"     let g:choice = choice 

    if !exists("file")
	exe "lcd " . fnameescape(cwd)
	return
    endif

    if file != "file_missing" && filereadable(file) && ( !exists("choice") || exists("choice") && choice != 0 )

	" Inherit tex flavour.
	" So that bib, cls, sty files will have their file type (bib/plaintex).
	let filetype	= &l:filetype
	let old_file	= expand("%:p")
	let atp_ErrorFormat	= b:atp_ErrorFormat
	let atp_LastLatexPID 	= ( exists("b:atp_LastLatexPID") 	? b:atp_LastLatexPID 	: 0 )
	let atp_LatexPIDs	= ( exists("b:atp_LatexPIDs") 		? b:atp_LatexPIDs 	: [] )
	let atp_BibtexPIDs	= ( exists("b:atp_BibtexPIDs") 		? b:atp_BibtexPIDs 	: [] )
	let atp_MakeindexPIDs	= ( exists("b:atp_MakeindexPIDs") 	? b:atp_MakeindexPIDs 	: [] )
	let atp_ProgressBar	= ( exists("b:atp_ProgressBar") 	? b:atp_ProgressBar 	: {} )
	execute "edit " . fnameescape(file)
	call RestoreProjectVariables(projectVarDict)
	if &l:filetype =~ 'tex$' && file =~ '\.tex$' && &l:filetype != filetype  
	    let &l:filetype	= filetype
	" If the filetype is 'bib' we should source some portion of ATP, so
	" that if the bib file is changed tex will process that file
	" 	The best approach is to source only compiler.vim and add an
	" 	autocommand.
" 	elseif &l:filetype == 'bib'
" 	    source ~/.vim/ftplugin/tex_atp.vim
	endif

	" Set the main file variable and pass the TreeOfFiles variables to the new
	" buffer.
	if exists("b:atp_ErrorFormat")
	    unlockvar b:atp_ErrorFormat
	endif
	let b:atp_ErrorFormat	= atp_ErrorFormat
	let [ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ]	= deepcopy([tree_d, file_l_orig, type_d, level_d ])
	if exists("b:atp_ProgressBar")
	    unlockvar b:atp_ProgressBar
	endif
	let [ b:atp_LastLatexPID, b:atp_LatexPIDs, b:atp_ProgressBar ] = [ atp_LastLatexPID, atp_LatexPIDs, atp_ProgressBar ]
	let [ b:atp_BibtexPIDs, b:atp_MakeindexPIDs ] = [ atp_BibtexPIDs, atp_MakeindexPIDs ]
	lockvar b:atp_ProgressBar
	if !&l:autochdir
	    exe "lcd " . fnameescape(cwd)
	endif
	return file
    else
	echohl ErrorMsg
	redraw
	if file != "file_missing"
	    echo "File \'".fname."\' not found."
	else
	    echo "Missing file."
	endif
	echohl None

	exe "lcd " . fnameescape(cwd)
	return file
    endif
endfunction
catch /E127:/
endtry
function! <SID>GotoFileComplete(ArgLead, CmdLine, CursorPos)
    let bang = ( a:CmdLine =~ '^\w*!' ? '!' : '')
    if bang == "!" || !exists("b:TreeOfFiles") || !exists("b:ListOfFiles") || !exists("b:TypeDict") || !exists("b:LevelDict") 
	let [tree_d, file_l, type_d, level_d ] 	= TreeOfFiles(atp_MainFile)
    else
	let [tree_d, file_l, type_d, level_d ] 	= deepcopy([ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ])
    endif
    if index(file_l, b:atp_MainFile) == -1 || index(file_l, fnamemodify(b:atp_MailFile, ":p")) == -1 
	call add(file_l, b:atp_MainFile) 
    endif
    return  filter(file_l, "v:val =~ a:ArgLead")
endfunction
" Skip Comment "{{{2
" a:flag=fb (f-forward, b-backward)
" f works like ]*
" b workd like [*
" Note: the 's' search flag is passed by the associated commands.
" This can be extended: 
" 	(1) skip empty lines between comments
function! <SID>SkipComment(flag, mode, ...)
    let flag 	= ( a:flag =~ 'b' ? 'b' : '' ) 
    let nr	= ( a:flag =~ 'b' ? '-1' : 1 )
    call search('^\zs\s*%', flag)
    call cursor(line("."), ( nr == -1 ? 1 : len(getline(line(".")))))

    let line	= getline(line("."))
    " find previous line
    let pline_nr=min([line("$"), max([1,line(".")+nr])])
    let pline	= getline(pline_nr) 
    " This code find previous non empty line    
"     while pline =~ '^\s*$' && pline_nr > 1 && pline_nr < line("$")
" 	let pline_nr += nr
" 	let pline=getline(pline_nr)
"     endwhile

"     while line =~ '^\s*%' || ( line =~ '^\s*$' && pline =~ '^\s*%' )
    while pline =~ '^\s*%'
	call cursor(line(".")+nr, ( nr == -1 ? 1 : len(getline(line(".")+nr))))
" 	let line=getline(line("."))
	let pline_nr=min([line("$"), max([1,line(".")+nr])])
	let pline	= getline(pline_nr) 
    endwhile
    if a:mode == 'v'
	let end_pos = [ line("."), col(".") ]
	" Go where visual mode started
	exe "normal `" . ( nr == 1 ? '<' : '>' ) 
	exe "normal " . visualmode()
	call cursor(end_pos)
    endif
endfunction

" Syntax motion
" {{{2 TexSyntaxMotion
function! TexSyntaxMotion(forward, how, ...)

    " If the function is used in imap.
    let in_imap	= ( a:0 >= 1 ? a:1 : 0 )

    let whichwrap	= split(&l:whichwrap, ',')
    if !count(whichwrap, 'l') 
	setl ww+=l
    endif
    if !count(whichwrap, 'h')
	setl ww+=h
    endif

    " before we use <Esc> 
    let line=line(".")
    if in_imap && len(getline(".")) > col(".")
	let col = col(".")+1
    else
	let col = col(".")
    endif
"     execute "normal l"
    let step 		= ( a:forward > 0 ? "l" : "h" )
    let synstack	= map(synstack(line, col), 'synIDattr( v:val, "name")')
    let synstackh	= map(synstack(line, max([1, col-1])), 'synIDattr( v:val, "name")')

    let DelimiterCount	= count(synstack, 'Delimiter') 
    let ScriptCount	= count(synstack, 'texSuperscript') + count(synstack, 'texSubscript')
    let ScriptsCount	= count(synstack, 'texSuperscripts') + count(synstack, 'texSubscripts')
    let StatementCount	= count(synstack, 'texStatement')
    let StatementCounth	= count(synstackh, 'texStatement') && col(".") > 1
    let SectionCount	= count(synstack, 'texSection')

    let TypeStyleCount	= count(synstack, 'texTypeStyle')
    let TypeStyleCounth	= count(synstackh, 'texTypeStyle') && col(".") > 1
    let MathTextCount	= count(synstack, 'texMathText')
    let MathTextCounth	= count(synstackh, 'texMathText') && col(".") > 1
    let RefZoneCount	= count(synstack, 'texRefZone')
    let RefZoneCounth	= count(synstackh, 'texRefZone') && col(".") > 1 
    let RefOptionCount	= count(synstack, 'texRefOption')
    let RefOptionCounth	= count(synstackh, 'texRefOption') && !count(synstackh, 'Delimiter') && col(".") > 1
    let CiteCount	= count(synstack, 'texCite')
    let CiteCounth	= count(synstackh, 'texCite') && !count(synstackh, 'Delimiter') && col(".") > 1
    let MatcherCount 	= count(synstack, 'texMatcher')
    let MatcherCounth 	= count(synstackh, 'texMatcher') && !count(synstackh, 'Delimiter') && col(".") > 1
    let MathMatcherCount 	= count(synstack, 'texMathMatcher')
    let MathMatcherCounth 	= count(synstackh, 'texMathMatcher') && !count(synstackh, 'Delimiter') && col(".") > 1
    let SectionNameCount 	= count(synstack, 'texSectionName')
    let SectionNameCounth 	= count(synstackh, 'texSectionName') && !count(synstackh, 'Delimiter') && col(".") > 1
    let SectionMarkerCount 	= count(synstack, 'texSectionMarker')

    let SectionModifierCount 	= count(synstack, 'texSectionModifier')
    let SectionModifierCounth 	= count(synstackh, 'texSectionModifier') && !count(synstackh, 'Delimiter') && col(".") > 1
"     let MathZonesCount		= len(filter(copy(synstack), 'v:val =~ ''^texMathZone[A-Z]'''))

"     let g:col	= col(".")
"     let g:line	= line(".")

    if DelimiterCount 
	let syntax	= [ 'Delimiter' ]
    elseif StatementCount && StatementCounth && step == "h"
	let syntax	= [ 'texStatement' ]
    elseif StatementCount && step != "h"
	let syntax	= [ 'texStatement' ]
    elseif SectionCount 
	let syntax	= [ 'texSection' ]
    elseif ScriptCount
	if a:how == 1
	    let syntax	= [ 'texSuperscript', 'texSubscript']
	else
	    let syntax	= [ 'texSuperscripts', 'texSubscripts']
	endif
    elseif TypeStyleCount && TypeStyleCounth && step == "h"
	let syntax	= [ 'texTypeStyle' ]
    elseif TypeStyleCount && step != "h"
	let syntax	= [ 'texTypeStyle' ]
    elseif RefZoneCount && RefZoneCounth && step == "h"
	let syntax	= [ 'texRefZone' ]
    elseif RefZoneCount && step != "h"
	let syntax	= [ 'texRefZone' ]
    elseif RefOptionCount && RefOptionCounth && step == "h"
	let syntax	= [ 'texRefOption' ]
    elseif RefOptionCount && step != "h"
	let syntax	= [ 'texRefOption' ]
    elseif CiteCount && CiteCounth && step == "h"
	let syntax	= [ 'texCite' ]
    elseif CiteCount && step != "h"
	let syntax	= [ 'texCite' ]
    elseif MatcherCount && MatcherCounth && step == "h"
	let syntax	= [ 'texMatcher' ]
    elseif MatcherCount && step != "h"
	let syntax	= [ 'texMatcher' ]
    elseif MathMatcherCount && MathMatcherCounth && step == "h"
	let syntax	= [ 'texMathMatcher' ]
    elseif MathMatcherCount && step != "h"
	let syntax	= [ 'texMathMatcher' ]
    elseif SectionNameCount && SectionNameCounth && step == "h"
	let syntax	= [ 'texSectionName' ]
    elseif SectionNameCount && step != "h"
	let syntax	= [ 'texSectionName' ]
    elseif SectionMarkerCount
	let syntax	= [ 'texSectionMarker' ]
    elseif SectionModifierCount && SectionModifierCounth && step == "h"
	let syntax	= [ 'texSectionModifier' ]
    elseif SectionModifierCount && step != "h"
	let syntax	= [ 'texSectionModifier' ]
    elseif MathTextCount && MathTextCounth && step == "h"
	let syntax	= [ 'texMathText' ]
    elseif MathTextCount && step != "h"
	let syntax	= [ 'texMathText' ]
"     elseif MathZonesCount
"     This might be slow
"     but we might change 'normal l' to 'normal w'
" 	let syntax	= [ 'texMathZoneA', 'texMathZoneB', 'texMathZoneC', 'texMathZoneD', 'texMathZoneE', 'texMathZoneF', 'texMathZoneG', 'texMathZoneH', 'texMathZoneI', 'texMathZoneJ', 'texMathZoneK', 'texMathZoneL', 'texMathZoneT', 'texMathZoneV', 'texMathZoneW', 'texMathZoneX', 'texMathZoneY' ]
    else
	" Go after first Delimiter
	let i=0
	let DelimiterCount	= count(synstack, 'Delimiter') 
	while !DelimiterCount
	    exe "normal " . step
	    let synstack	= map(synstack(line("."), col(".")), 'synIDattr( v:val, "name")')
	    let DelimiterCount	= count(synstack, 'Delimiter') 
	    if i == 1
		let DelimiterCount = 0
	    endif
	    let i+=1
	endwhile
	if in_imap
	    normal a
	endif
	return "Delimiter motion"
    endif

    let true	= 0
    for syn in syntax
	let true += count(synstack, syn)
    endfor
    let initial_count	= true

    while true >= initial_count
	let true	= 0
	execute "normal " . step
	let synstack	= map(synstack(line("."), col(".")), 'synIDattr( v:val, "name")')
	for syn in syntax
	    let true += count(synstack, syn)
	endfor
    endwhile
    while getline(".")[col(".")] =~ '^{\|}\|(\|)\|\[\|\]$'
	exe "normal l"
    endwhile
    if getline(".")[col(".")-2] == "{"
	exe "normal h"
    endif
    let &l:whichwrap	= join(whichwrap, ',')
    if in_imap
	normal a
"     else
" 	normal l
    endif
    if step == "l" && syntax == [ 'Delimiter' ]
	normal h
    endif
endfunction

" ctrl-j motion
" {{{2 ctrl-j motion
" New <Ctrl-j> motion
function! JMotion(flag)
" 	Note: pattern to match only commands which do not have any arguments:
" 	'\(\\\w\+\>\s*{\)\@!\\\w\+\>'
    if a:flag !~# 'b'
	let pattern = '\%(\]\zs\|{\zs\|}\zs\|(\zs\|)\zs\|\[\zs\|\]\zs\|\$\zs\|^\zs\s*$\|\(\\\w\+\>\s*{\)\@!\\\w\+\>\zs\)'
    else
	let pattern = '\%(\]\|{\|}\|(\|)\|\[\|\]\|\$\|^\s*$\|\(\\\w\+\>\s*{\)\@!\\\w\+\>\)'
    endif
    if getline(line(".")) =~ '&'
	let pattern = '\%(&\s*\zs\|^\s*\zs\)\|' . pattern
    endif

    "     let g:col = col(".") " sometimes this doesn't work - in normal mode go to
    "     end of line and press 'a' - then col(".") is not working!
"     let g:let = getline(line("."))[col(".")-1]
"     let g:con = getline(line("."))[col(".")-1] =~ '\%(\$\|{\|}\|(\|)\|\[\|\]\)' && col(".") < len(getline(line(".")))
    if getline(line("."))[col(".")-1] =~ '\%(\$\|{\|}\|(\|)\|\[\|\]\)' && a:flag !~# 'b'
	if col(".") == len(getline(line(".")))
	    execute "normal a "
	else
	    call cursor(line("."), col(".")+1)
	endif
	return
    else
	call search(pattern, a:flag)
	" In the imaps we use 'a' for the backward move and 'i' for forward move! 
	let condition = getline(line("."))[col(".")-1] =~ '\%(\$\|{\|}\|(\|)\|\[\|\]\)'
	if a:flag !~# 'b' && col(".") == len(getline(line("."))) && condition
" 	    Add a space at the end of line and move there
		execute "normal a "
	endif
    endif
endfunction
endif
" }}}1

" Commands And Maps:
augroup ATP_BufList
    " Add opened files to t:atp_toc_buflist.
    au!
    au BufEnter *.tex call s:buflist()
augroup END
" {{{1
if exists(":Tags") != 2
    command! -buffer -bang Tags						:call <SID>LatexTags(<q-bang>)
else
    command! -buffer -bang LatexTags					:call <SID>LatexTags(<q-bang>)
endif
command! -nargs=? -complete=custom,RemoveFromToCComp RemoveFromToC	:call RemoveFromToC(<q-args>)
map	<buffer> <silent> <Plug>JumptoPreviousEnvironment		:call <SID>JumptoEnvironment(1)<CR>
map	<buffer> <silent> <Plug>JumptoNextEnvironment			:call <SID>JumptoEnvironment(0)<CR>
command! -buffer -count=1 Part		:call <SID>ggGotoSection(<q-count>, 'part')
command! -buffer -count=1 Chap		:call <SID>ggGotoSection(<q-count>, 'chapter')
command! -buffer -count=1 Sec		:call <SID>ggGotoSection(<q-count>, 'section')
command! -buffer -count=1 SSec		:call <SID>ggGotoSection(<q-count>, 'subsection')

command! -buffer -nargs=1 -complete=custom,<SID>CompleteDestinations GotoNamedDest	:call <SID>GotoNamedDestination(<f-args>)
command! -buffer SkipCommentForward  	:call <SID>SkipComment('fs', 'n')
command! -buffer SkipCommentBackward 	:call <SID>SkipComment('bs', 'n')
vmap <buffer> <Plug>SkipCommentForward	:call <SID>SkipComment('fs', 'v')<CR>
vmap <buffer> <Plug>SkipCommentBackward	:call <SID>SkipComment('bs', 'v', col("."))<CR>

imap <Plug>TexSyntaxMotionForward	<Esc>:call TexSyntaxMotion(1,1,1)<CR>a
imap <Plug>TexSyntaxMotionBackward	<Esc>:call TexSyntaxMotion(0,1,1)<CR>a
nmap <Plug>TexSyntaxMotionForward	:call TexSyntaxMotion(1,1)<CR>
nmap <Plug>TexSyntaxMotionBackward	:call TexSyntaxMotion(0,1)<CR>

imap <Plug>TexJMotionForward	<Esc><Right>:call JMotion('')<CR>i
imap <Plug>TexJMotionBackward	<Esc>:call JMotion('b')<CR>a
nmap <Plug>TexJMotionForward	:call JMotion('')<CR>
nmap <Plug>TexJMotionBackward	:call JMotion('b')<CR>

command! -buffer -nargs=1 -complete=buffer MakeToc	:echo s:maketoc(fnamemodify(<f-args>, ":p"))[fnamemodify(<f-args>, ":p")] 
command! -buffer -bang -nargs=? TOC	:call <SID>TOC(<q-bang>)
command! -buffer CTOC			:call CTOC()
command! -buffer -bang Labels		:call <SID>Labels(<q-bang>)
command! -buffer -count=1 -nargs=? -complete=customlist,EnvCompletionWithoutStarEnvs Nenv	:call <SID>GotoEnvironment('sW',<q-count>,<q-args>)  | let v:searchforward=1 
command! -buffer -count=1 -nargs=? -complete=customlist,EnvCompletionWithoutStarEnvs Penv	:call <SID>GotoEnvironment('bsW',<q-count>,<q-args>) | let v:searchforward=0
"TODO: These two commands should also work with sections.
command! -buffer -count=1 -nargs=? -complete=custom,F_compl F	:call <SID>GotoEnvironment('sW',<q-count>,<q-args>)  | let v:searchforward=1
command! -buffer -count=1 -nargs=? -complete=custom,F_compl B	:call <SID>GotoEnvironment('bsW',<q-count>,<q-args>) | let v:searchforward=0

nnoremap <silent> <buffer> <Plug>GotoNextEnvironment		:<C-U>call <SID>GotoEnvironment('sW',v:count1,'')<CR>
nnoremap <silent> <buffer> <Plug>GotoPreviousEnvironment	:<C-U>call <SID>GotoEnvironment('bsW',v:count1,'')<CR>

nnoremap <silent> <buffer> <Plug>GotoNextMath			:<C-U>call <SID>GotoEnvironment('sW',v:count1,'math')<CR>
nnoremap <silent> <buffer> <Plug>GotoPreviousMath		:<C-U>call <SID>GotoEnvironment('bsW',v:count1,'math')<CR>

nnoremap <silent> <buffer> <Plug>GotoNextInlineMath		:<C-U>call <SID>GotoEnvironment('sW',v:count1,'inlinemath')<CR>
nnoremap <silent> <buffer> <Plug>GotoPreviousInlineMath		:<C-U>call <SID>GotoEnvironment('bsW',v:count1,'inlinemath')<CR>

nnoremap <silent> <buffer> <Plug>GotoNextDisplayedMath	 	:<C-U>call <SID>GotoEnvironment('sW',v:count1,'displayedmath')<CR>
nnoremap <silent> <buffer> <Plug>GotoPreviousDisplayedMath	:<C-U>call <SID>GotoEnvironment('bsW',v:count1,'displayedmath')<CR>

if &l:cpoptions =~# 'B'
    nnoremap <silent> <Plug>GotoNextSubSection		:<C-U>call <SID>GotoSection("", v:count1, "s", "\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>", ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', '')<CR>
    onoremap <silent> <Plug>GotoNextSubSection		:<C-U>call <SID>GotoSection("", v:count1, "s","\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>", 'vim')<CR>
    vnoremap <silent> <Plug>vGotoNextSubSection		m':<C-U>exe "normal! gv"<Bar>exe "normal! w"<Bar>call search('^\([^%]\\|\\\@<!\\%\)*\\\%(subsection\\|section\\|chapter\\|part\)\*\=\>\\|\\end\s*{\s*document\s*}', 'W')<Bar>exe "normal! b"<CR>

    nnoremap <silent> <Plug>GotoNextSection		:<C-U>call <SID>GotoSection("", v:count1, "s", "\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>", ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', '')<CR>
    onoremap <silent> <Plug>GotoNextSection		:<C-U>call <SID>GotoSection("", v:count1, "s", "\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>", 'vim')<CR>
    vnoremap <silent> <Plug>vGotoNextSection		m':<C-U>exe "normal! gv"<Bar>exe "normal! w"<Bar>call search('^\([^%]\\|\\\@<!\\%\)*\\\%(section\\|chapter\\|part\)\*\=\>\\|\\end\s*{\s*document\s*}', 'W')<Bar>exe "normal! b"<CR>

    nnoremap <silent> <Plug>GotoNextChapter		:<C-U>call <SID>GotoSection("", v:count1, "s", "\\\\\\%(chapter\\\\|part\\)\\*\\=\\>", ( g:atp_mapNn ? 'atp' : 'vim' ))<CR>
    onoremap <silent> <Plug>GotoNextChapter		:<C-U>call <SID>GotoSection("", v:count1, "s", "\\\\\\%(chapter\\\\|part\\)\\*\\=\\>", 'vim')<CR>
    vnoremap <silent> <Plug>vGotoNextChapter		m':<C-U>exe "normal! gv"<Bar>exe "normal! w"<Bar>call search('^\([^%]\\|\\\@<!\\%\)*\\\%(chapter\\|part\)\*\=\>\\|\\end\s*{\s*document\s*}', 'W')<Bar>exe "normal! b"<CR>

    nnoremap <silent> <Plug>GotoNextPart		:<C-U>call <SID>GotoSection("", v:count1, "s", "\\\\part\\*\\=\\>", ( g:atp_mapNn ? 'atp' : 'vim' ), 'n')<CR>
    onoremap <silent> <Plug>GotoNextPart		:<C-U>call <SID>GotoSection("", v:count1, "s", "\\\\part\\*\\=\\>", 'vim', 'n')<CR>
    vnoremap <silent> <Plug>vGotoNextPart		m':<C-U>exe "normal! gv"<Bar>exe "normal! w"<Bar>call search('^\([^%]\\|\\\@<!\\%\)*\\\%(part\*\=\>\\|\\end\s*{\s*document\s*}\)', 'W')<Bar>exe "normal! b"<CR>
else
    nnoremap <silent> <Plug>GotoNextSubSection		:<C-U>call <SID>GotoSection("", v:count1, "s", '\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', '')<CR>
    onoremap <silent> <Plug>GotoNextSubSection		:<C-U>call <SID>GotoSection("", v:count1, "s",'\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>', 'vim')<CR>
    vnoremap <silent> <Plug>vGotoNextSubSection		m':<C-U>exe "normal! gv"<Bar>exe "normal! w"<Bar>call search('^\\([^%]\\\\|\\\\\\@<!\\\\%\\)*\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>\\\\|\\\\end\\s*{\\s*document\\s*}', 'W')<Bar>exe "normal! b"<CR>

    nnoremap <silent> <Plug>GotoNextSection		:<C-U>call <SID>GotoSection("", v:count1, "s", '\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', '')<CR>
    onoremap <silent> <Plug>GotoNextSection		:<C-U>call <SID>GotoSection("", v:count1, "s", '\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>', 'vim')<CR>
    vnoremap <silent> <Plug>vGotoNextSection		m':<C-U>exe "normal! gv"<Bar>exe "normal! w"<Bar>call search('^\\([^%]\\\\|\\\\\\@<!\\\\%\\)*\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>\\\\|\\\\end\\s*{\\s*document\\s*}', 'W')<Bar>exe "normal! b"<CR>

    nnoremap <silent> <Plug>GotoNextChapter		:<C-U>call <SID>GotoSection("", v:count1, "s", '\\\\\\%(chapter\\\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ))<CR>
    onoremap <silent> <Plug>GotoNextChapter		:<C-U>call <SID>GotoSection("", v:count1, "s", '\\\\\\%(chapter\\\\|part\\)\\*\\=\\>', 'vim')<CR>
    vnoremap <silent> <Plug>vGotoNextChapter		m':<C-U>exe "normal! gv"<Bar>exe "normal! w"<Bar>call search('^\\([^%]\\\\|\\\\\\@<!\\\\%\\)*\\\\\\%(chapter\\\\|part\\)\\*\\=\\>\\\\|\\\\end\\s*{\\s*document\\s*}', 'W')<Bar>exe "normal! b"<CR>

    nnoremap <silent> <Plug>GotoNextPart		:<C-U>call <SID>GotoSection("", v:count1, "s", '\\\\part\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n')<CR>
    onoremap <silent> <Plug>GotoNextPart		:<C-U>call <SID>GotoSection("", v:count1, "s", '\\\\part\\*\\=\\>', 'vim', 'n')<CR>
    vnoremap <silent> <Plug>vGotoNextPart		m':<C-U>exe "normal! gv"<Bar>exe "normal! w"<Bar>call search('^\\([^%]\\\\|\\\\\\@<!\\\\%\\)*\\\\\\%(part\\*\\=\\>\\\\|\\\\end\\s*{\\s*document\\s*}\\)', 'W')<Bar>exe "normal! b"<CR>
endif

command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl NSSSec		:call <SID>GotoSection(<q-bang>, <q-count>, "s", '\\\%(subsubsection\|subsection\|section\|chapter\|part\)\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl NSSec		:call <SID>GotoSection(<q-bang>, <q-count>, "s", '\\\%(subsection\|section\|chapter\|part\)\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl NSec		:call <SID>GotoSection(<q-bang>, <q-count>, "s", '\\\%(section\|chapter\|part\)\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl NChap		:call <SID>GotoSection(<q-bang>, <q-count>, "s", '\\\%(chapter\|part\)\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl NPart		:call <SID>GotoSection(<q-bang>, <q-count>, "s", '\\part\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)

if &l:cpoptions =~# 'B'
    nnoremap <silent> <Plug>GotoPreviousSubSection	:<C-U>call <SID>GotoSection("", v:count1, "sb", "\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>", ( g:atp_mapNn ? 'atp' : 'vim' ), 'n')<CR>
    onoremap <silent> <Plug>GotoPreviousSubSection	:<C-U>call <SID>GotoSection("", v:count1, "sb", "\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>", 'vim')<CR>
    vnoremap <silent> <Plug>vGotoPreviousSubSection	m':<C-U>exe "normal! gv"<Bar>call search('\\\%(subsection\\|section\\|chapter\\|part\)\*\=\>\\|\\begin\s*{\s*document\s*}', 'bW')<CR>

    nnoremap <silent> <Plug>GotoPreviousSection	:<C-U>call <SID>GotoSection("", v:count1, "sb", "\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>", ( g:atp_mapNn ? 'atp' : 'vim' ), 'n')<CR>
    onoremap <silent> <Plug>GotoPreviousSection	:<C-U>call <SID>GotoSection("", v:count1, "sb", "\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>", 'vim')<CR>
    vnoremap <silent> <Plug>vGotoPreviousSection	m':<C-U>exe "normal! gv"<Bar>call search('\\\%(section\\|chapter\\|part\)\*\=\>\\|\\begin\s*{\s*document\s*}', 'bW')<CR>

    nnoremap <silent> <Plug>GotoPreviousChapter	:<C-U>call <SID>GotoSection("", v:count1, "sb", "\\\\\\%(chapter\\\\|part\\)\\>", ( g:atp_mapNn ? 'atp' : 'vim' ))<CR>
    onoremap <silent> <Plug>GotoPreviousChapter	:<C-U>call <SID>GotoSection("", v:count1, "sb", "\\\\\\%(chapter\\\\|part\\)\\>", 'vim')<CR
    vnoremap <silent> <Plug>vGotoPreviousChapter	m':<C-U>exe "normal! gv"<Bar>call search('\\\%(chapter\\|part\)\*\=\>\\|\\begin\s*{\s*document\s*}', 'bW')<CR>

    nnoremap <silent> <Plug>GotoPreviousPart	:<C-U>call <SID>GotoSection("", v:count1, "sb", "\\\\part\\*\\=\\>", ( g:atp_mapNn ? 'atp' : 'vim' ))<CR>
    onoremap <silent> <Plug>GotoPreviousPart	:<C-U>call <SID>GotoSection("", v:count1, "sb", "\\\\part\\*\\=\\>", 'vim')<CR>
    vnoremap <silent> <Plug>vGotoPreviousPart	m':<C-U>exe "normal! gv"<Bar>call search('\\\%(part\*\=\)\>', 'bW')<CR>
else
    nnoremap <silent> <Plug>GotoPreviousSubSection	:<C-U>call <SID>GotoSection("", v:count1, "sb", '\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n')<CR>
    onoremap <silent> <Plug>GotoPreviousSubSection	:<C-U>call <SID>GotoSection("", v:count1, "sb", '\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>', 'vim')<CR>
    vnoremap <silent> <Plug>vGotoPreviousSubSection	m':<C-U>exe "normal! gv"<Bar>call search('\\\\\\%(subsection\\\\|section\\\\|chapter\\\\|part\\)\\*\\=\\>\\\\|\\\\begin\\s*{\\s*document\\s*}', 'bW')<CR>

    nnoremap <silent> <Plug>GotoPreviousSection	:<C-U>call <SID>GotoSection("", v:count1, "sb", '\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n')<CR>
    onoremap <silent> <Plug>GotoPreviousSection	:<C-U>call <SID>GotoSection("", v:count1, "sb", '\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>', 'vim')<CR>
    vnoremap <silent> <Plug>vGotoPreviousSection	m':<C-U>exe "normal! gv"<Bar>call search('\\\\\\%(section\\\\|chapter\\\\|part\\)\\*\\=\\>\\\\|\\\\begin\\s*{\\s*document\\s*}', 'bW')<CR>

    nnoremap <silent> <Plug>GotoPreviousChapter	:<C-U>call <SID>GotoSection("", v:count1, "sb", '\\\\\\%(chapter\\\\|part\\)\\>', ( g:atp_mapNn ? 'atp' : 'vim' ))<CR>
    onoremap <silent> <Plug>GotoPreviousChapter	:<C-U>call <SID>GotoSection("", v:count1, "sb", '\\\\\\%(chapter\\\\|part\\)\\>', 'vim')<CR
    vnoremap <silent> <Plug>vGotoPreviousChapter	m':<C-U>exe "normal! gv"<Bar>call search('\\\\\\%(chapter\\\\|part\\)\\*\\=\\>\\\\|\\\\begin\\s*{\\s*document\\s*}', 'bW')<CR>

    nnoremap <silent> <Plug>GotoPreviousPart	:<C-U>call <SID>GotoSection("", v:count1, "sb", '\\\\part\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ))<CR>
    onoremap <silent> <Plug>GotoPreviousPart	:<C-U>call <SID>GotoSection("", v:count1, "sb", '\\\\part\\*\\=\\>', 'vim')<CR>
    vnoremap <silent> <Plug>vGotoPreviousPart	m':<C-U>exe "normal! gv"<Bar>call search('\\\\\\%(part\\*\\=\\)\\>', 'bW')<CR>
endif


if &l:cpoptions =~# 'B'
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PSSSec		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\%(\%(sub\)\{1,2}section\|section\|chapter\|part\)\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PSSec		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\%(subsection\|section\|chapter\|part\)\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PSec		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\%(section\|chapter\|part\)\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PChap		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\%(chapter\|part\)\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PPart		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\part\*\=\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
else
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PSSSec		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\\\\%(\\%(sub\\)\\{1,2}section\\|section\\|chapter\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PSSec		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\\\\%(subsection\\|section\\|chapter\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PSec		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\\\\%(section\\|chapter\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PChap		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\\\\%(chapter\\|part\\)\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
    command! -buffer -bang -count=1 -nargs=? -complete=customlist,Env_compl PPart		:call <SID>GotoSection(<q-bang>, <q-count>, 'sb', '\\\\part\\*\\=\\>', ( g:atp_mapNn ? 'atp' : 'vim' ), 'n', <q-args>)
endif

command! -buffer NInput				:call <SID>Input("w") 	| let v:searchforward = 1
command! -buffer PInput 			:call <SID>Input("bw")	| let v:searchforward = 0
command! -buffer -nargs=? -bang -complete=customlist,<SID>GotoFileComplete GotoFile	:call GotoFile(<q-bang>,<q-args>, 0)
command! -buffer -nargs=? -bang -complete=customlist,<SID>GotoFileComplete EditInputFile :call GotoFile(<q-bang>,<q-args>, 0)
" vimeif data[0]['text'] =~ 'No Unique Match Found'	    echohl WarningMsg
command! -bang -nargs=? -complete=customlist,GotoLabelCompletion GotoLabel  		:call GotoLabel(<f-bang>, <f-args>)
