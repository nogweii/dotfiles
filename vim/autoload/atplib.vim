" Title: 	Vim library for ATP filetype plugin.
" Author:	Marcin Szamotulski
" Email:	mszamot [AT] gmail [DOT] com
" Note:		This file is a part of Automatic Tex Plugin for Vim.
" URL:		https://launchpad.net/automatictexplugin
" Language:	tex

" Kill:
" {{{
function! atplib#KillPIDs(pids,...)
    if len(a:pids) == 0 && a:0 == 0
	return
    endif
python << END
import os, signal
from signal import SIGKILL
pids=vim.eval("a:pids")
for pid in pids:
    try:
	os.kill(int(pid),SIGKILL)
    except OSError, e:
	if e.errno == 3:
             # No such process error.
             pass
        else:
             raise
END
endfunction " }}}
" Write:
function! atplib#write(...) "{{{
    let backup		= &backup
    let writebackup	= &writebackup
    let project		= b:atp_ProjectScript

    " Disable WriteProjectScript
    let b:atp_ProjectScript = 0
    set nobackup
    set nowritebackup

    if a:0 > 0 && a:1 == "silent"
	silent! update
    else
	update
    endif

    let b:atp_ProjectScript = project
    let &backup		= backup
    let &writebackup	= writebackup
endfunction "}}}
" Log:
function! atplib#Log(file, string, ...) "{{{1
    if a:0 >= 1
	call delete(g:atp_TempDir."/".a:file)
    else
	exe "redir >> ".g:atp_TempDir."/".a:file 
	silent echo a:string
	redir END
    endif
endfunction "}}}1
" Outdir: append to '/' to b:atp_OutDir if it is not present. 
function! atplib#outdir() "{{{1
    if has("win16") || has("win32") || has("win64") || has("win95")
	if b:atp_OutDir !~ "\/$"
	    let b:atp_OutDir=b:atp_OutDir . "\\"
	endif
    else
	if b:atp_OutDir !~ "\/$"
	    let b:atp_OutDir=b:atp_OutDir . "/"
	endif
    endif
    return b:atp_OutDir
endfunction
"}}}1
" Return {path} relative to {rel}, if not under {rel} return {path}
function! atplib#RelativePath(path, rel) "{{{1
    let current_dir 	= getcwd()
    exe "lcd " . fnameescape(a:rel)
    let rel_path	= fnamemodify(a:path, ':.')
    exe "lcd " . fnameescape(current_dir)
    return rel_path
endfunction
"}}}1
" Return fullpath
function! atplib#FullPath(file_name) "{{{1
    let cwd = getcwd()
    if a:file_name =~ '^\s*\/'
	let file_path = a:file_name
    elseif exists("b:atp_ProjectDir")
	exe "lcd " . fnameescape(b:atp_ProjectDir)
	let file_path = fnamemodify(a:file_name, ":p")
	exe "lcd " . fnameescape(cwd)
    else
	let file_path = fnamemodify(a:file_name, ":p")
    endif
    return file_path
endfunction
"}}}1
" Table:
"{{{ atplibTable, atplib#FormatListinColumns, atplib#PrintTable
function! atplib#Table(list, spaces)
" take a list of lists and make a list which is nicely formated (to echo it)
" spaces = list of spaces between columns.
    "maximal length of columns:
    let max_list=[]
    let new_list=[]
    for i in range(len(a:list[0]))
	let max=max(map(deepcopy(a:list), "len(v:val[i])"))
	call add(max_list, max)
    endfor

    for row in a:list
	let new_row=[]
	let i=0
	for el in row
	    let new_el=el.join(map(range(max([0,max_list[i]-len(el)+get(a:spaces,i,0)])), "' '"), "")
	    call add(new_row, new_el)
	    let i+=1
	endfor
	call add(new_list, new_row)
    endfor

    return map(new_list, "join(v:val, '')")
endfunction 
function! atplib#FormatListinColumns(list,s)
    " take a list and reformat it into many columns
    " a:s is the number of spaces between columns
    " for example of usage see atplib#PrintTable
    let max_len=max(map(copy(a:list), 'len(v:val)'))
    let new_list=[]
    let k=&l:columns/(max_len+a:s)
    let len=len(a:list)
    let column_len=len/k
    for i in range(0, column_len)
	let entry=[]
	for j in range(0,k)
	    call add(entry, get(a:list, i+j*(column_len+1), ""))
	endfor
	call add(new_list,entry)
    endfor
    return new_list
endfunction 
" Take list format it with atplib#FormatListinColumns and then with
" atplib#Table (which makes columns of equal width)
function! atplib#PrintTable(list, spaces)
    " a:list 	- list to print
    " a:spaces 	- nr of spaces between columns 

    let list = atplib#FormatListinColumns(a:list, a:spaces)
    let nr_of_columns = max(map(copy(list), 'len(v:val)'))
    let spaces_list = ( nr_of_columns == 1 ? [0] : map(range(1,nr_of_columns-1), 'a:spaces') )

    return atplib#Table(list, spaces_list)
endfunction
"}}}

" QFLength "{{{
function! atplib#qflength() 
    let lines = 1
    " i.e. open with one more line than needed.
    for qf in getqflist()
	let text=substitute(qf['text'], '\_s\+', ' ', 'g')
	let lines+=(len(text))/&l:columns+1
    endfor
    return lines
endfunction "}}}

function! atplib#Let(varname, varvalue)
    exe "let ".substitute(string(a:varname), "'", "", "g")."=".substitute(string(a:varvalue), "''\\@!", "", "g")
endfunction

" IMap Functions:
" {{{
" These maps extend ideas from TeX_9 plugin:
function! atplib#IsInMath()
    return atplib#CheckSyntaxGroups(g:atp_MathZones) && 
		    \ !atplib#CheckSyntaxGroups(['texMathText'])
endfunction
function! atplib#MakeMaps(maps, ...)
    let aucmd = ( a:0 >= 1 ? a:1 : '' )
    for map in a:maps
	if map[3] != "" && ( !exists(map[5]) || {map[5]} > 0 || 
		    \ exists(map[5]) && {map[5]} == 0 && aucmd == 'InsertEnter'  )
	    if exists(map[5]) && {map[5]} == 0 && aucmd == 'InsertEnter'
		exe "let ".map[5]." =1"
	    endif
	    exe map[0]." ".map[1]." ".map[2].map[3]." ".map[4]
	endif
    endfor
endfunction
function! atplib#DelMaps(maps)
    for map in a:maps
	let cmd = matchstr(map[0], '[^m]\ze\%(nore\)\=map') . "unmap"
	let arg = ( map[1] =~ '<buffer>' ? '<buffer>' : '' )
	try
	    exe cmd." ".arg." ".map[2].map[3]
	catch /E31:/
	endtry
    endfor
endfunction
" From TeX_nine plugin:
function! atplib#IsLeft(lchar,...)
    let nr = ( a:0 >= 1 ? a:1 : 0 )
    let left = getline('.')[col('.')-2-nr]
    if left ==# a:lchar
	return 1
    else
	return 0
    endif
endfunction
" try
function! atplib#ToggleIMaps(var, augroup, ...)
    if atplib#IsInMath() 
	call atplib#MakeMaps(a:var, a:augroup)
    else
	call atplib#DelMaps(a:var)
	if a:0 >= 1
	    call atplib#MakeMaps(a:1)
	endif
    endif
endfunction
" catch E127
" endtry "}}}

" Compilation Call Back Communication: 
" with some help of D. Munger
" (Communications with compiler script: both in compiler.vim and the python script.)
" {{{ Compilation Call Back Communication
" TexReturnCode {{{
function! atplib#TexReturnCode(returncode)
	let b:atp_TexReturnCode=a:returncode
endfunction "}}}
" BibtexReturnCode {{{
function! atplib#BibtexReturnCode(returncode,...)
	let b:atp_BibtexReturnCode=a:returncode
	let b:atp_BibtexOutput= ( a:0 >= 1 ? a:1 : "" )
endfunction
" }}}
" Callback {{{
" a:mode 	= a:verbose 	of s:compiler ( one of 'default', 'silent',
" 				'debug', 'verbose')
" a:commnad	= a:commmand 	of s:compiler 
"		 		( a:commnad = 'AU' if run from background)
"
" Uses b:atp_TexReturnCode which is equal to the value returned by tex
" compiler.
function! atplib#CallBack(mode,...)

    " If the compiler was called by autocommand.
    let AU = ( a:0 >= 1 ? a:1 : 'COM' )
    " Was compiler called to make bibtex
    let BIBTEX = ( a:0 >= 2 ? a:2 : "False" )
    let BIBTEX = ( BIBTEX == "True" || BIBTEX == 1 ? 1 : 0 )
    if g:atp_debugCallBack
	exe "redir! > ".g:atp_TempDir."/CallBack.log"
    endif

    for cmd in keys(g:CompilerMsg_Dict) 
    if b:atp_TexCompiler =~ '^\s*' . cmd . '\s*$'
	    let Compiler 	= g:CompilerMsg_Dict[cmd]
	    break
	else
	    let Compiler 	= b:atp_TexCompiler
	endif
    endfor
    let b:atp_running	= b:atp_running - 1

    " Read the log file
    cgetfile
    if g:atp_debugCallBack
	silent echo "file=".expand("%:p")
	silent echo "g:atp_HighlightErrors=".g:atp_HighlightErrors
    endif
    if g:atp_HighlightErrors
	call atplib#HighlightErrors()
    endif
    " /this cgetfile is not working (?)/
    let error	= len(getqflist()) + (BIBTEX ? b:atp_BibtexReturnCode : 0)

    " If the log file is open re read it / it has 'autoread' opion set /
    checktime

    " redraw the status line /for the notification to appear as fast as
    " possible/ 
    if a:mode != 'verbose'
	redrawstatus
    endif

    " redraw has values -0,1 
    "  1 do  not redraw 
    "  0 redraw
    "  i.e. redraw at the end of function (this is done to not redraw twice in
    "  this function)
    let l:clist 	= 0
    let atp_DebugMode = t:atp_DebugMode

    if b:atp_TexReturnCode == 0 && ( a:mode == 'silent' || atp_DebugMode == 'silent' ) && g:atp_DebugMode_AU_change_cmdheight 
	let &l:cmdheight=g:atp_cmdheight
    endif

    if g:atp_debugCallBack
	let g:debugCB 		= 0
	let g:debugCB_mode 	= a:mode
	let g:debugCB_error 	= error
	silent echo "mode=".a:mode."\nerror=".error
    endif

    let msg_list = []
    let showed_message = 0

    if a:mode == "silent" && !error

	if t:atp_QuickFixOpen 

	    if g:atp_debugCallBack
		let g:debugCB .= 7
	    endif

	    cclose
	    call add(msg_list, ["[ATP:] no errors, closing quick fix window.", "Normal"])
	endif
    endif

    if a:mode ==? 'debug' && !error

	if g:atp_debugCallBack
	    let g:debugCB 	.= 3
	endif

	cclose
	call add(msg_list,["[ATP:] ".b:atp_TexCompiler." returned without errors [b:atp_ErrorFormat=".b:atp_ErrorFormat."]".(g:atp_DefaultDebugMode=='silent'&&atp_DebugMode!='silent'?"\ngoing out of debuging mode.": "."), "Normal", "after"]) 
	let showed_message 	= 1
	let t:atp_DebugMode 	= g:atp_DefaultDebugMode
	if g:atp_DefaultDebugMode == "silent" && t:atp_QuickFixOpen
	    cclose
	endif
	let &l:cmdheight 	= g:atp_cmdheight
    endif

    " debug mode with errors
    if a:mode ==? 'debug' && error
	if len(getqflist())

	    if g:atp_debugCallBack
		let g:debugCB .= 4
	    endif

	    let &l:cmdheight 	= g:atp_DebugModeCmdHeight
		let showed_message 	= 1
		if b:atp_ReloadOnError || b:atp_Viewer !~ '^\s*xpdf\>'
		    call add(msg_list, ["[ATP:] ".Compiler." returned with exit code " . b:atp_TexReturnCode . ".", (b:atp_TexReturnCode ? "ErrorMsg" : "Normal"), "before"])
		else
		    call add(msg_list, ["[ATP:] ".Compiler." returned with exit code " . b:atp_TexReturnCode . " output file not reloaded.", (b:atp_TexReturnCode ? "ErrorMsg" : "Normal"), "before"])
		endif
	    if !t:atp_QuickFixOpen
		let l:clist		= 1
	    endif
	endif

	if BIBTEX && b:atp_BibtexReturnCode

	    if g:atp_debugCallBack
		let g:debugCB .= 8
	    endif

	    let l:clist		= 1
	    call add(msg_list, [ "[Bib:] BibTeX returned with exit code ".b:atp_BibtexReturnCode .".", "ErrorMsg", "after"])
	    call add(msg_list, [ "BIBTEX_OUTPUT" , "Normal", "after"])

	endif

	" In debug mode, go to first error. 
	if a:mode ==# "Debug"

	    if g:atp_debugCallBack
		let g:debugCB .= 6
	    endif

	    cc
	endif
    endif

    if msg_list == []
	if g:atp_debugCallBack
	    redir END
	endif
	return
    endif

    " Count length of the message:
    let msg_len		= len(msg_list)
    if len(map(copy(msg_list), "v:val[0] == 'BIBTEX_OUTPUT'")) 
	let msg_len += (BIBTEX ? len(split(b:atp_BibtexOutput, "\\n")) - 1 : - 1 )
    endif
    let msg_len		+= ((len(getqflist()) <= 7 && !t:atp_QuickFixOpen) ? len(getqflist()) : 0 )

    " Show messages/clist
    
    if g:atp_debugCallBack
	let g:msg_list 	= msg_list
	let g:clist 	= l:clist
	silent echo "msg_list=\n**************\n".join(msg_list, "\n")."\n**************"
	silent echo "l:clist=".l:clist
    endif

    let cmdheight = &l:cmdheight
    let &l:cmdheight	= msg_len+2
    if l:clist && len(getqflist()) > 7 && !t:atp_QuickFixOpen
	let winnr = winnr()
	copen
	exe winnr."wincmd w"
    elseif (a:mode ==? "debug") && !t:atp_QuickFixOpen 
	let l:clist = 1
    endif
    redraw
    let before_msg = filter(copy(msg_list), "v:val[2] == 'before'")
    let after_msg = filter(copy(msg_list), "v:val[2] == 'after'")
    for msg in before_msg 
	exe "echohl " . msg[1]
	echo msg[0]
    endfor
    let l:redraw	= 1
    if l:clist && len(getqflist()) <= 7 && !t:atp_QuickFixOpen
	if g:atp_debugCallBack
	    let g:debugCB .= "clist"
	endif
	try
	    clist
	catch E42:
	endtry
	let l:redraw	= 0
    endif
    for msg in after_msg 
	exe "echohl " . msg[1]
	if msg[0] !=# "BIBTEX_OUTPUT"
	    echo msg[0]
	else
	    echo "       ".substitute(b:atp_BibtexOutput, "\n", "\n       ", "g")
	    let g:debugCB .=" BIBTEX_output "
	endif
    endfor
    echohl Normal
    if len(msg_list)==0
	redraw
    endif
    let &l:cmdheight = cmdheight
    if g:atp_debugCallBack
	redir END
    endif
endfunction "}}}
"{{{ LatexPID
"Store LatexPIDs in a variable
function! atplib#LatexPID(pid)
    call add(b:atp_LatexPIDs, a:pid)
"     call atplib#PIDsRunning("b:atp_BitexPIDs")
    let b:atp_LastLatexPID =a:pid
endfunction "}}}
"{{{ BibtexPID
"Store BibtexPIDs in a variable
function! atplib#BibtexPID(pid)
    call add(b:atp_BibtexPIDs, a:pid)
"     call atplib#PIDsRunning("b:atp_BibtexPIDs")
endfunction "}}}
"{{{ MakeindexPID
"Store MakeindexPIDs in a variable
function! atplib#MakeindexPID(pid)
    call add(b:atp_MakeindexPIDs, a:pid)
    let b:atp_LastMakeindexPID =a:pid
endfunction "}}}
"{{{ PythonPID
"Store PythonPIDs in a variable
function! atplib#PythonPID(pid)
    call add(b:atp_PythonPIDs, a:pid)
"     call atplib#PIDsRunning("b:atp_PythonPIDs")
endfunction "}}}
"{{{ MakeindexPID
"Store MakeindexPIDs in a variable
function! atplib#PythonPIDs(pid)
    call add(b:atp_PythonPIDs, a:pid)
    let b:atp_LastPythonPID =a:pid
endfunction "}}}
"{{{ PIDsRunning
function! atplib#PIDsRunning(var)
" a:var is a string, and might be one of 'b:atp_LatexPIDs', 'b:atp_BibtexPIDs' or
" 'b:atp_MakeindexPIDs'
python << EOL
import psutil, re, sys, vim
var  = vim.eval("a:var")
pids = vim.eval(var)
if len(pids) > 0:
    ps_list=psutil.get_pid_list()
    rmpids=[]
    for lp in pids:
	run=False
	for p in ps_list:
            if str(lp) == str(p):
		run=True
		break
	if not run:
            rmpids.append(lp)
    rmpids.sort()
    rmpids.reverse()
    for pid in rmpids:
	vim.eval("filter("+var+", 'v:val !~ \""+str(pid)+"\"')")
EOL
endfunction "}}}
"{{{ ProgressBar
function! atplib#ProgressBar(value,pid)
    unlockvar b:atp_ProgressBar
    if a:value != 'end'
	let b:atp_ProgressBar[a:pid]=a:value
    else
	call remove(b:atp_ProgressBar, a:pid)
    endif
    lockvar b:atp_ProgressBar
    redrawstatus
"     redraw
"     echomsg a:value
endfunction "}}}
"{{{ redrawstatus
function! atplib#redrawstatus()
    redrawstatus
endfunction "}}}
"{{{ CursorMoveI
" function! atplib#CursorMoveI()
"     if mode() != "i"
" 	return
"     endif
"     let cursor_pos=[ line("."), col(".")]
"     call feedkeys("\<left>", "n")
"     call cursor(cursor_pos)
" endfunction "}}}
" {{{ HighlightErrors
function! atplib#HighlightErrors()
    call atplib#ClearHighlightErrors()
    let qf_list = getqflist()
    for error in qf_list
	if error.type ==? 'e'
	    let hlgroup = g:atp_Highlight_ErrorGroup
	else
	    let hlgroup = g:atp_Highlight_WarningGroup
	endif
	if hlgroup == ""
	    continue
	endif
	let m_id = matchadd(hlgroup, '\%'.error.lnum.'l.*', 20)
	call add(s:matchid, m_id)
	let error_msg=split(error.text, "\n")
    endfor
endfunction "}}}
" {{{ ClearHighlightErrors
function! atplib#ClearHighlightErrors()
    if !exists("s:matchid")
	let s:matchid=[]
	return
    endif
    for m_id in s:matchid
	try
	    silent call matchdelete(m_id)
	catch /E803:/
	endtry
    endfor
    let s:matchid=[]
endfunction "}}}
"{{{ echo
function! atplib#Echo(msg, cmd, hlgroup, ...)
    if a:0 >= 1 && a:1
	redraw
    endif
    exe "echohl ".a:hlgroup
    exe a:cmd." '".a:msg."'"
    echohl Normal
endfunction "}}}
" }}}

" Toggle On/Off Completion 
" {{{1 atplib#OnOffComp
function! atplib#OnOffComp(ArgLead, CmdLine, CursorPos)
    return filter(['on', 'off'], 'v:val =~ "^" . a:ArgLead') 
endfunction
"}}}1
" Open Function:
 "{{{1 atplib#Open
 " a:1	- pattern or a file name
 " 		a:1 is regarded as a filename if filereadable(pattern) is non
 " 		zero.
function! atplib#Open(bang, dir, TypeDict, ...)
    if a:dir == "0"
	echohl WarningMsg 
	echomsg "You have to set g:atp_LibraryPath in your vimrc or atprc file." 
	echohl Normal
	return
    endif

    let pattern = ( a:0 >= 1 ? a:1 : "") 
    let file	= filereadable(pattern) ? pattern : ""

    if file == ""
	if a:bang == "!" || !exists("g:atp_Library")
	    let g:atp_Library 	= filter(split(globpath(a:dir, "*"), "\n"), 'count(keys(a:TypeDict), fnamemodify(v:val, ":e"))')
	    let found 		= deepcopy(g:atp_Library) 
	else
	    let found		= deepcopy(g:atp_Library)
	endif
	call filter(found, "fnamemodify(v:val, ':t') =~ pattern")
	" Resolve symlinks:
	call map(found, "resolve(v:val)")
	" Remove double entries:
	call filter(found, "count(found, v:val) == 1")
	if len(found) > 1
	    echohl Title 
	    echo "Found files:"
	    echohl Normal
	    let i = 1
	    for file in found
		if len(map(copy(found), "v:val =~ escape(fnamemodify(file, ':t'), '~') . '$'")) == 1
		    echo i . ") " . fnamemodify(file, ":t")
		else
		    echo i . ") " . pathshorten(fnamemodify(file, ":p"))
		endif
		let i+=1
	    endfor
	    let choice = input("Which file to open? ")-1
	    if choice == -1
		return
	    endif
	    let file = found[choice]
	elseif len(found) == 1
	    let file = found[0]
	else
	    echohl WarningMsg
	    echomsg "[ATP:] Nothing found."
	    echohl None
	    return
	endif
    endif

    let ext 	= fnamemodify(file, ":e")
    let viewer 	= get(a:TypeDict, ext, 0) 

    if viewer == '0'
	echomsg "\n"
	echomsg "[ATP:] filetype: " . ext . " is not supported, add an entry to g:atp_OpenTypeDict" 
	return
    endif
    if viewer !~ '^\s*cat\s*$' && viewer !~ '^\s*g\=vim\s*$' && viewer !~ '^\s*edit\s*$' && viewer !~ '^\s*tabe\s*$' && viewer !~ '^\s*split\s*$'
	call system(viewer . " '" . file . "' &")  
    elseif viewer =~ '^\s*g\=vim\s*$' || viewer =~ '^\s*tabe\s*$'
	exe "tabe " . fnameescape(file)
	setl nospell
    elseif viewer =~ '^\s*edit\s*$' || viewer =~ '^\s*split\s*$'
	exe viewer . " " . fnameescape(file)
	setl nospell
    elseif viewer == '^\s*cat\s*'
	redraw!
	echohl Title
	echo "cat '" . file . "'"
	echohl Normal
	echo system(viewer . " '" . file . "' &")  
    endif
"     if fnamemodify(file, ":t") != "" && count(g:atp_open_completion, fnamemodify(file, ":t")) == 0
" 	call extend(g:atp_open_completion, [fnamemodify(file, ":t")], 0)
"     endif
    " This removes the hit Enter vim prompt. 
    call feedkeys("<CR>")
    return
endfunction
"}}}1

" Find Vim Server: find server 'hosting' a file and move to the line.
" {{{1 atplib#FindAndOpen
" Can be used to sync gvim with okular.
" just set in okular:
" 	settings>okular settings>Editor
" 		Editor		Custom Text Editor
" 		Command		gvim --servername GVIM --remote-expr "atplib#FindAndOpen('%f','%l', '%c')"
" You can also use this with vim but you should start vim with
" 		vim --servername VIM
" and use servername VIM in the Command above.		
function! atplib#ServerListOfFiles()
    exe "redir! > " . g:atp_TempDir."/ServerListOfFiles.log"
    let file_list = []
    for nr in range(1, bufnr('$')-1)
	let files 	= getbufvar(nr, "ListOfFiles")
	let main_file 	= getbufvar(nr, "atp_MainFile")
	if string(files) != "" 
	    call add(file_list, main_file)
	endif
	if string(main_file) != ""
	    call extend(file_list, files)
	endif
    endfor
    call filter(file_list, 'v:val != ""')
    call map(file_list, "fnamemodify(v:val, ':p')") 
    redir end
    return file_list
endfunction
function! atplib#FindAndOpen(file, line, ...)
    let col		= ( a:0 >= 1 ? a:1 : 1 )
    let file		= ( fnamemodify(a:file, ":e") == "tex" ? a:file : fnamemodify(a:file, ":p:r") . ".tex" )
    let server_list	= split(serverlist(), "\n")
    exe "redir! >".g:atp_TempDir."/FindAndOpen.log"
    echo "server list=".string(server_list)
    if len(server_list) == 0
	return 1
    endif
    let use_server	= "no_server"
    for server in server_list
	let file_list=split(remote_expr(server, 'atplib#ServerListOfFiles()'), "\n")
	echo "server " .server . " " . string(file_list)
	if index(file_list, file) != -1
	    let use_server	= server
	    break
	endif
    endfor
    if use_server == "no_server"
	let use_server=server_list[0]
    endif
    echo "file:".file." line:".a:line. " col ".col." server name:".use_server." hitch-hiking server:".v:servername 
    call system(v:progname." --servername ".use_server." --remote-wait +".a:line." ".fnameescape(file) . " &")
    call remote_expr(use_server, 'cursor('.a:line.','.col.')')
    call remote_expr(use_server, 'redraw!')
"   call system(v:progname." --servername ".use_server." --remote-exprt \"remote_foreground('".use_server."')\"")
"   This line is not working in DWM, but it might work in KDE (to be tested):
"     call system(v:progname." --servername ".use_server." --remote-exprt foreground\(\)")
    redir end
    return "File:".file." line:".a:line." col:".col." server name:".use_server." Hitch-hiking server:".v:servername 
endfunction
"}}}1

" Labels Tools: GrepAuxFile, SrotLabels, generatelabels and showlabes.
" {{{1 LABELS
" the argument should be: resolved full path to the file:
" resove(fnamemodify(bufname("%"),":p"))

" {{{2 --------------- atplib#GrepAuxFile
function! atplib#GrepAuxFile(...)
    " Aux file to read:
    if exists("b:atp_MainFile")
	let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    endif
    let aux_filename	= ( a:0 == 0 && exists("b:atp_MainFile") ? fnamemodify(atp_MainFile, ":r") . ".aux" : a:1 )
    let tex_filename	= fnamemodify(aux_filename, ":r") . ".tex"

    if !filereadable(aux_filename)
	" We should worn the user that there is no aux file
	" /this is not visible ! only after using the command 'mes'/
	echohl WarningMsg
	echomsg "[ATP:] there is no aux file. Run ".b:atp_TexCompiler." first."
	echohl Normal
	return []
	" CALL BACK is not working
	" I can not get output of: vim --servername v:servername --remote-expr v:servername
	" for v:servername
	" Here we should run latex to produce auxfile
" 	echomsg "Running " . b:atp_TexCompiler . " to get aux file."
" 	let labels 	= system(b:atp_TexCompiler . " -interaction nonstopmode " . atp_MainFile . " 1&>/dev/null  2>1 ; " . " vim --servername ".v:servername." --remote-expr 'atplib#GrepAuxFile()'")
" 	return labels
    endif
"     let aux_file	= readfile(aux_filename)

    let saved_llist	= getloclist(0)
    if bufloaded(aux_filename)
	exe "silent! bd! " . bufnr(aux_filename)
    endif
    try
	silent execute 'lvimgrep /\\newlabel\s*{/j ' . fnameescape(aux_filename)
    catch /E480:/
    endtry
    let loc_list	= getloclist(0)
    call setloclist(0, saved_llist)
    call map(loc_list, ' v:val["text"]')

    let labels		= []
    if g:atp_debugGAF
	let g:gaf_debug	= {}
    endif

    " Equation counter depedns on the option \numberwithin{equation}{section}
    " /now this only supports article class.
    let equation = len(atplib#GrepPreambule('^\s*\\numberwithin{\s*equation\s*}{\s*section\s*}', tex_filename))
"     for line in aux_file
    for line in loc_list
" 	if line =~ '^\\newlabel' 
	    " line is of the form:
	    " \newlabel{<label>}{<rest>}
	    " where <rest> = {<label_number}{<title>}{<counter_name>.<counter_number>}
	    " <counter_number> is usually equal to <label_number>.
	    "
	    " Document classes: article, book, amsart, amsbook, review:
	    " NEW DISCOVERY {\zs\%({[^}]*}\|[^}]\)*\ze} matches for inner part of 
	    " 	{ ... { ... } ... }	/ only one level of being recursive / 
	    " 	The order inside the main \%( \| \) is important.
	    "This is in the case that the author put in the title a command,
	    "for example \mbox{...}, but not something more difficult :)
	    if line =~ '^\\newlabel{[^}]*}{{[^}]*}{[^}]*}{\%({[^}]*}\|[^}]\)*}{[^}]*}'
		let debug	= 1
		let label	= matchstr(line, '^\\newlabel\s*{\zs[^}]*\ze}')
		let rest	= matchstr(line, '^\\newlabel\s*{[^}]*}\s*{\s*{\zs.*\ze}\s*$')
		let l:count = 1
		let i	= 0
		while l:count != 0 
		    let l:count = ( rest[i] == '{' ? l:count+1 : rest[i] == '}' ? l:count-1 : l:count )
		    let i+= 1
		endwhile
		let number	= substitute(strpart(rest,0,i-1), '{\|}', '', 'g')  
		let rest	= strpart(rest,i)
		let rest	= substitute(rest, '^{[^}]*}{', '', '')
		let l:count = 1
		let i	= 0
		while l:count != 0 
		    let l:count = rest[i] == '{' ? l:count+1 : rest[i] == '}' ? l:count-1 : l:count 
		    let i+= 1
		endwhile
		let counter	= substitute(strpart(rest,i-1), '{\|}', '', 'g')  
		let counter	= strpart(counter, 0, stridx(counter, '.')) 

	    " Document classes: article, book, amsart, amsbook, review
	    " (sometimes the format is a little bit different)
	    elseif line =~ '\\newlabel{[^}]*}{{\d\%(\d\|\.\)*{\d\%(\d\|\.\)*}}{\d*}{\%({[^}]*}\|[^}]\)*}{[^}]*}'
		let debug	= 2
		let list = matchlist(line, 
		    \ '\\newlabel{\([^}]*\)}{{\(\d\%(\d\|\.\)*{\d\%(\d\|\.\)*\)}}{\d*}{\%({[^}]*}\|[^}]\)*}{\([^}]*\)}')
	    	let [ label, number, counter ] = [ list[1], list[2], list[3] ]
		let number	= substitute(number, '{\|}', '', 'g')
		let counter	= matchstr(counter, '^\w\+')

	    " Document class: article
	    elseif line =~ '\\newlabel{[^}]*}{{\d\%(\d\|\.\)*}{\d\+}}'
		let debug	= 3
		let list = matchlist(line, '\\newlabel{\([^}]*\)}{{\(\d\%(\d\|\.\)*\)}{\d\+}}')
	    	let [ label, number, counter ] = [ list[1], list[2], "" ]

	    " Memoir document class uses '\M@TitleReference' command
	    " which doesn't specify the counter number.
	    elseif line =~ '\\M@TitleReference' 
		let debug	= 4
		let label	= matchstr(line, '^\\newlabel\s*{\zs[^}]*\ze}')
		let number	= matchstr(line, '\\M@TitleReference\s*{\zs[^}]*\ze}') 
		let counter	= ""

	    elseif line =~ '\\newlabel{[^}]*}{.*\\relax\s}{[^}]*}{[^}]*}}'
		" THIS METHOD MIGHT NOT WORK WELL WITH: book document class.
		let debug	= 5.0
		let label 	= matchstr(line, '\\newlabel{\zs[^}]*\ze}{.*\\relax\s}{[^}]*}{[^}]*}}')
		let nc 		= matchstr(line, '\\newlabel{[^}]*}{.*\\relax\s}{\zs[^}]*\ze}{[^}]*}}')
		let counter	= matchstr(nc, '\zs\a*\ze\(\.\d\+\)\+')
		let number	= matchstr(nc, '.*\a\.\zs\d\+\(\.\d\+\)\+') 
		if counter == 'equation' && !equation
		    let number = matchstr(number, '\d\+\.\zs.*')
		endif

	    " aamas2010 class
	    elseif line =~ '\\newlabel{[^}]*}{{\d\%(\d\|.\)*{\d\%(\d\|.\)*}{[^}]*}}' && atplib#DocumentClass(b:atp_MainFile) =~? 'aamas20\d\d'
		let debug	= 5.1
		let label 	= matchstr(line, '\\newlabel{\zs[^}]*\ze}{{\d\%(\d\|.\)*{\d\%(\d\|.\)*}{[^}]*}}')
		let number 	= matchstr(line, '\\newlabel{\zs[^}]*\ze}{{\zs\d\%(\d\|.\)*{\d\%(\d\|.\)*\ze}{[^}]*}}')
		let number	= substitute(number, '{\|}', '', 'g')
		let counter	= ""

	    " subeqautions
	    elseif line =~ '\\newlabel{[^}]*}{{[^}]*}{[^}]*}}'
		let debug	= 6
		let list = matchlist(line, '\\newlabel{\([^}]*\)}{{\([^}]*\)}{\([^}]*\)}}')
		let [ label, number ] = [ list[1], list[2] ]
		let counter	= ""

	    " AMSBook uses \newlabel for tocindent
	    " which we filter out here.
	    else
		let debug	= 7
		let label	= "nolabel: " . line
	    endif

	    if label !~ '^nolabel:\>'
		call add(labels, [ label, number, counter, debug])
	    endif
	    if g:atp_debugGAF
		call extend(g:gaf_debug, { label : [ number, counter, debug ] })
	    endif
" 	endif
    endfor

    return labels
endfunction
" }}}2
" Sorting function used to sort labels.
" {{{2 --------------- atplib#SortLabels
" It compares the first component of lists (which is line number)
" This should also use the bufnr.
function! atplib#SortLabels(list1, list2)
    if a:list1[0] == a:list2[0]
	return 0
    elseif str2nr(a:list1[0]) > str2nr(a:list2[0])
	return 1
    else
	return -1
    endif
endfunction
" }}}2
" Function which find all labels and related info (label number, lable line
" number, {bufnr} <= TODO )
" {{{2 --------------- atplib#generatelabels
" This function runs in two steps:
" 	(1) read lables from aux files using GrepAuxFile()
" 	(2) search all input files (TreeOfFiles()) for labels to get the line
" 		number 
" 	   [ this is done using :vimgrep which is fast, when the buffer is not loaded ]
function! atplib#generatelabels(filename, ...)
    let s:labels	= {}
    let bufname		= fnamemodify(a:filename,":t")
    let auxname		= fnamemodify(a:filename,":p:r") . ".aux"
    let return_ListOfFiles	= a:0 >= 1 ? a:1 : 1

    let true=1
    let i=0

    let aux_labels	= atplib#GrepAuxFile(auxname)

    let saved_pos	= getpos(".")
    call cursor(1,1)

    let [ TreeofFiles, ListOfFiles, DictOfFiles, LevelDict ] 		= TreeOfFiles(a:filename)
    let ListOfFiles_orig = copy(ListOfFiles)
    if count(ListOfFiles, a:filename) == 0
	call add(ListOfFiles, a:filename)
    endif
    let saved_llist	= getloclist(0)
    call setloclist(0, [])

    " Look for labels in all input files.
    for file in ListOfFiles
	let file	= atplib#FullPath(file)
	silent! execute "lvimgrepadd /\\label\s*{/j " . fnameescape(file)
    endfor
    let loc_list	= getloclist(0)
"     call setloclist(0, saved_llist)
    call map(loc_list, '[ v:val["lnum"], v:val["text"], v:val["bufnr"] ]')

    let labels = {}

    for label in aux_labels
	let dict		= filter(copy(loc_list), "v:val[1] =~ '\\label\s*{\s*'.escape(label[0], '*\/$.') .'\s*}'")
	let line		= get(get(dict, 0, []), 0, "") 
	let bufnr		= get(get(dict, 0, []), 2, "")
	let bufname		= fnamemodify(bufname(bufnr), ":p")
	if get(labels, bufname, []) == []
	    let labels[bufname] = [ [line, label[0], label[1], label[2], bufnr ] ]
	else
	    call add(labels[bufname], [line, label[0], label[1], label[2], bufnr ]) 
	endif
    endfor

    for bufname in keys(labels)
	call sort(labels[bufname], "atplib#SortLabels")
    endfor

"     let i=0
"     while i < len(texfile)
" 	if texfile[i] =~ '\\label\s*{'
" 	    let lname 	= matchstr(texfile[i], '\\label\s*{.*', '')
" 	    let start 	= stridx(lname, '{')+1
" 	    let lname 	= strpart(lname, start)
" 	    let end	= stridx(lname, '}')
" 	    let lname	= strpart(lname, 0, end)
"     "This can be extended to have also the whole environment which
"     "could be shown.
" 	    call extend(s:labels, { i+1 : lname })
" 	endif
" 	let i+=1 
"     endwhile

    if exists("t:atp_labels")
	call extend(t:atp_labels, labels, "force")
    else
	let t:atp_labels	= labels
    endif
    keepjumps call setpos(".", saved_pos)
    if return_ListOfFiles
	return [ t:atp_labels, ListOfFiles_orig ]
    else
	return t:atp_labels
    endif
endfunction
" }}}2
" This function opens a new window and puts the results there.
" {{{2 --------------- atplib#showlabels
" the argument is [ t:atp_labels, ListOfFiles ] 
" 	where ListOfFiles is the list returne by TreeOfFiles() 
function! atplib#showlabels(labels)

    " the argument a:labels=t:atp_labels[bufname("")] !
    let l:cline=line(".")

    let saved_pos	= getpos(".")

    " Open new window or jump to the existing one.
    let l:bufname	= bufname("")
    let l:bufpath	= fnamemodify(resolve(fnamemodify(bufname("%"),":p")),":h")
    let BufFullName	= fnamemodify(l:bufname, ":p") 

    let l:bname="__Labels__"

    let t:atp_labelswinnr=winnr()
    let t:atp_labelsbufnr=bufnr("^".l:bname."$")
    let l:labelswinnr=bufwinnr(t:atp_labelsbufnr)

    let tabstop	= 0
    for file in a:labels[1]
	let dict	= get(a:labels[0], file, [])
	let tabstop	= max([tabstop, max(map(copy(dict), "len(v:val[2])")) + 1])
	unlet dict
    endfor
"     let g:labelswinnr	= l:labelswinnr
    let saved_view	= winsaveview()

    if l:labelswinnr != -1
	" Jump to the existing window.
	redraw
	exe l:labelswinnr . " wincmd w"
	if l:labelswinnr != t:atp_labelswinnr
	    silent exe "%delete"
	else
	    echoerr "ATP error in function s:showtoc, TOC/LABEL buffer 
		    \ and the tex file buffer agree."
	    return
	endif
    else

    " Open new window if its width is defined (if it is not the code below
    " will put lab:cels in the current buffer so it is better to return.
	if !exists("t:atp_labels_window_width")
	    echoerr "t:atp_labels_window_width not set"
	    return
	endif

	" tabstop option is set to be the longest counter number + 1
	redraw
	let l:openbuffer= "keepalt " . t:atp_labels_window_width . "vsplit +setl\\ tabstop=" . tabstop . "\\ nowrap\\ buftype=nofile\\ filetype=toc_atp\\ syntax=labels_atp __Labels__"
	silent exe l:openbuffer
	silent call atplib#setwindow()
	let t:atp_labelsbufnr=bufnr("")
    endif
    unlockvar b:atp_Labels
    let b:atp_Labels	= {}

    let g:labels=copy(a:labels)

    let line_nr	= 2
    for file in a:labels[1]
	if !(len(get(a:labels[0], file, []))>0)
	    continue
	endif
	call setline("$", fnamemodify(file, ":t") . " (" . fnamemodify(file, ":h")  . ")")
	call extend(b:atp_Labels, { 1 : [ file, 0 ]})
	for label in get(a:labels[0], file, [])
	    " Set line in the format:
	    " /<label_numberr> \t[<counter>] <label_name> (<label_line_nr>)/
	    " if the <counter> was given in aux file (see the 'counter' variable in atplib#GrepAuxFile())
	    " print it.
	    " /it is more complecated because I want to make it as tight as
	    " possible and as nice as possible :)
	    " the first if checks if there are counters, then counter type is
	    " printed, then the tabs are set./
    " 	let slen	= winwidth(0)-tabstop-5-5
    " 	let space_len 	= max([1, slen-len(label[1])])
	    if tabstop+(len(label[3][0])+3)+len(label[1])+(len(label[0])+2) < winwidth(0)
		let space_len	= winwidth(0)-(tabstop+(len(label[3][0])+3)+len(label[1])+(len(label[0])+2))
	    else
		let space_len  	= 1
	    endif
	    let space	= join(map(range(space_len), '" "'), "")
	    let set_line 	= label[2] . "\t[" . label[3][0] . "] " . label[1] . space . "(" . label[0] . ")"
	    call setline(line_nr, set_line ) 
	    call extend(b:atp_Labels, { line_nr : [ file, label[0] ]}) 
	    let line_nr+=1
	endfor
    endfor
    lockvar 3 b:atp_Labels

    " set the cursor position on the correct line number.
    call search(l:bufname, 'w')
    let l:number=1
    for label  in get(a:labels[0], BufFullName, [])
	if l:cline >= label[0]
	    keepjumps call cursor(line(".")+1, col("."))
	elseif l:number == 1 && l:cline < label[0]
	    keepjumps call cursor(line(".")+1, col("."))
	endif
	let l:number+=1
    endfor
endfunction
" }}}2
" }}}1

" Table Of Contents Tools:
function! atplib#getlinenr(...) "{{{
    let line 	=  a:0 >= 1 ? a:1 : line('.')
    let labels 	=  a:0 >= 2 ? a:2 : expand("%") == "__Labels__" ? 1 : 0

    if labels == 0
	return get(b:atp_Toc, line, ["", ""])[1]
    else
	return get(b:atp_Labels, line, ["", ""])[1]
    endif
endfunction "}}}
function! atplib#CursorLine() "{{{
    if exists("t:cursorline_idmatch")
	try
	    call matchdelete(t:cursorline_idmatch)
	catch /E803:/
	endtry
    endif
    if atplib#getlinenr(line(".")) 
	let t:cursorline_idmatch =  matchadd('CursorLine', '^\%'.line(".").'l.*$')
    endif
endfunction "}}}

" Various Comparing Functions:
"{{{1 atplib#CompareNumbers
function! atplib#CompareNumbers(i1, i2)
   return str2nr(a:i1) == str2nr(a:i2) ? 0 : str2nr(a:i1) > str2nr(a:i2) ? 1 : -1
endfunction
"}}}1
" {{{1 atplib#CompareCoordinates
" Each list is an argument with two values:
" listA=[ line_nrA, col_nrA] usually given by searchpos() function
" listB=[ line_nrB, col_nrB]
" returns 1 iff A is smaller than B
fun! atplib#CompareCoordinates(listA,listB)
    if a:listA[0] < a:listB[0] || 
	\ a:listA[0] == a:listB[0] && a:listA[1] < a:listB[1] ||
	\ a:listA == [0,0]
	" the meaning of the last is that if the searchpos() has not found the
	" beginning (a:listA) then it should return 1 : the env is not started.
	return 1
    else
	return 0
    endif
endfun
"}}}1
" {{{1 atplib#CompareCoordinates_leq
" Each list is an argument with two values!
" listA=[ line_nrA, col_nrA] usually given by searchpos() function
" listB=[ line_nrB, col_nrB]
" returns 1 iff A is smaller or equal to B
function! atplib#CompareCoordinates_leq(listA,listB)
    if a:listA[0] < a:listB[0] || 
	\ a:listA[0] == a:listB[0] && a:listA[1] <= a:listB[1] ||
	\ a:listA == [0,0]
	" the meaning of the last is that if the searchpos() has not found the
	" beginning (a:listA) then it should return 1 : the env is not started.
	return 1
    else
	return 0
    endif
endfunction
"}}}1
" ReadInputFile function reads finds a file in tex style and returns the list
" of its lines. 
" {{{1 atplib#ReadInputFile
" this function looks for an input file: in the list of buffers, under a path if
" it is given, then in the b:atp_OutDir.
" directory. The last argument if equal to 1, then look also
" under g:texmf.
function! atplib#ReadInputFile(ifile,check_texmf)

    let l:input_file = []

    " read the buffer or read file if the buffer is not listed.
    if buflisted(fnamemodify(a:ifile,":t"))
	let l:input_file=getbufline(fnamemodify(a:ifile,":t"),1,'$')
    " if the ifile is given with a path it should be tried to read from there
    elseif filereadable(a:ifile)
	let l:input_file=readfile(a:ifile)
    " if not then try to read it from b:atp_OutDir
    elseif filereadable(b:atp_OutDir . fnamemodify(a:ifile,":t"))
	let l:input_file=readfile(filereadable(b:atp_OutDir . fnamemodify(a:ifile,":t")))
    " the last chance is to look for it in the g:texmf directory
    elseif a:check_texmf && filereadable(findfile(a:ifile,g:texmf . '**'))
	let l:input_file=readfile(findfile(a:ifile,g:texmf . '**'))
    endif

    return l:input_file
endfunction
"}}}1

" BIB SEARCH:
" These are all bibsearch realted variables and functions.
"{{{ BIBSEARCH
"{{{ atplib#variables
let atplib#bibflagsdict={ 
                \ 't' : ['title',       'title        '],               'a' : ['author',        'author       '], 
		\ 'b' : ['booktitle',   'booktitle    '],               'c' : ['mrclass',       'mrclass      '], 
		\ 'e' : ['editor',      'editor       '], 	        'j' : ['journal',       'journal      '], 
		\ 'f' : ['fjournal',    'fjournal     '], 	        'y' : ['year',          'year         '], 
		\ 'n' : ['number',      'number       '], 	        'v' : ['volume',        'volume       '], 
		\ 's' : ['series',      'series       '], 	        'p' : ['pages',         'pages        '], 
		\ 'P' : ['publisher',   'publisher    '],               'N' : ['note',          'note         '], 
		\ 'S' : ['school',      'school       '], 	        'h' : ['howpublished',  'howpublished '], 
		\ 'o' : ['organization', 'organization '],              'I' : ['institution' ,  'institution '],
		\ 'u' : ['url',         'url          '],
		\ 'H' : ['homepage',    'homepage     '], 	        'i' : ['issn',          'issn         '],
		\ 'k' : ['key',         'key          '], 	        'R' : ['mrreviewer',    'mrreviewer   ']}
" they do not work in the library script :(
" using g:bibflags... .
" let atplib#bibflagslist=keys(atplib#bibflagsdict)
" let atplib#bibflagsstring=join(atplib#bibflagslist,'')
"}}}
" This is the main search engine.
"{{{ atplib#searchbib
" ToDo should not search in comment lines.

" To make it work after kpsewhich is searching for bib path.
" let s:bibfiles=FindBibFiles(bufname('%'))
function! atplib#searchbib(pattern, bibdict, ...) 

    call atplib#outdir()
    " for tex files this should be a flat search.
    let flat 	= &filetype == "plaintex" ? 1 : 0
    let bang	= a:0 >=1 ? a:1 : ""
    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)

    " Make a pattern which will match for the elements of the list g:bibentries
    let pattern = '^\s*@\%(\<'.g:bibentries[0].'\>'
    for bibentry in g:bibentries['1':len(g:bibentries)]
	let pattern	= pattern . '\|\<' . bibentry . '\>'
    endfor
    let pattern	= pattern . '\)'
" This pattern matches all entry lines: author = \| title = \| ... 
    let pattern_b = '^\s*\%('
    for bibentry in keys(g:bibflagsdict)
	let pattern_b	= pattern_b . '\|\<' . g:bibflagsdict[bibentry][0] . '\>'
    endfor
    let pattern_b.='\)\s*='

    if g:atp_debugBS
	exe "redir! >>".g:atp_TempDir."/BibSearch.log"
	silent! echo "==========atplib#searchbib==================="
	silent! echo "atplib#searchbib_bibfiles=" . string(s:bibfiles)
	silent! echo "a:pattern=" . a:pattern
	silent! echo "pattern=" . pattern
	silent! echo "pattern_b=" . pattern_b
	silent! echo "bang=" . bang
	silent! echo "flat=" . flat
    endif

    unlet bibentry
    let b:bibentryline={} 
    
    " READ EACH BIBFILE IN TO DICTIONARY s:bibdict, WITH KEY NAME BEING THE bibfilename
    let s:bibdict={}
    let l:bibdict={}
    for l:f in keys(a:bibdict)
	let s:bibdict[l:f]=[]

	" read the bibfile if it is in b:atp_OutDir or in g:atp_raw_bibinputs directory
	" ToDo: change this to look in directories under g:atp_raw_bibinputs. 
	" (see also ToDo in FindBibFiles 284)
" 	for l:path in split(g:atp_raw_bibinputs, ',') 
" 	    " it might be problem when there are multiple libraries with the
" 	    " same name under different locations (only the last one will
" 	    " survive)
" 	    let s:bibdict[l:f]=readfile(fnameescape(findfile(atplib#append(l:f,'.bib'), atplib#append(l:path,"/") . "**")))
" 	endfor
	let l:bibdict[l:f]=copy(a:bibdict[l:f])
	" clear the s:bibdict values from lines which begin with %    
	call filter(l:bibdict[l:f], ' v:val !~ "^\\s*\\%(%\\|@\\cstring\\)"')
    endfor

    if g:atp_debugBS
	silent! echo "values(l:bibdict) len(l:bibdict[v:val]) = " . string(map(deepcopy(l:bibdict), "len(v:val)"))
    endif

    if a:pattern != ""
	for l:f in keys(a:bibdict)
	    let l:list=[]
	    let l:nr=1
	    for l:line in l:bibdict[l:f]
		" Match Pattern:
		" if the line matches find the beginning of this bib field and add its
		" line number to the list l:list
		" remove ligatures and brackets {,} from the line
		let line_without_ligatures = substitute(substitute(l:line,'\C{\|}\|\\\%("\|`\|\^\|=\|\.\|c\|\~\|v\|u\|d\|b\|H\|t\)\s*','','g'), "\\\\'\\s*", '', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\oe', 'oe', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\OE', 'OE', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\ae', 'ae', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\AE', 'AE', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\o', 'o', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\O', 'O', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\i', 'i', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\j', 'j', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\l', 'l', 'g')
		let line_without_ligatures = substitute(line_without_ligatures, '\C\\L', 'L', 'g')

		if line_without_ligatures =~? a:pattern

		    if g:atp_debugBS
			silent! echo "line_without_ligatures that matches " . line_without_ligatures
			silent! echo "____________________________________"
		    endif

		    let l:true=1
		    let l:t=0
		    while l:true == 1
			let l:tnr=l:nr-l:t

			    if g:atp_debugBS
				silent! echo " l:tnr=" . string(l:tnr) . " l:bibdict[". string(l:f) . "][" . string(l:tnr-1) . "]=" . string(l:bibdict[l:f][l:tnr-1])
			    endif

			" go back until the line will match pattern (which
			" should be the beginning of the bib field.
		       if l:bibdict[l:f][l:tnr-1] =~? pattern && l:tnr >= 0
			   let l:true=0
			   let l:list=add(l:list,l:tnr)
		       elseif l:tnr <= 0
			   let l:true=0
		       endif
		       let l:t+=1
		    endwhile
		endif
		let l:nr+=1
	    endfor

	    if g:atp_debugBS
		silent! echo "A l:list=" . string(l:list)
	    endif

    " CLEAR THE l:list FROM ENTRIES WHICH APPEAR TWICE OR MORE --> l:clist
	    let l:pentry="A"		" We want to ensure that l:entry (a number) and l:pentry are different
	    for l:entry in l:list
		if l:entry != l:pentry
		    if count(l:list,l:entry) > 1
			while count(l:list,l:entry) > 1
			    let l:eind=index(l:list,l:entry)
			    call remove(l:list,l:eind)
			endwhile
		    endif 
		    let l:pentry=l:entry
		endif
	    endfor

	    " This is slower than the algorithm above! 
" 	    call sort(filter(l:list, "count(l:list, v:val) == 1"), "atplib#CompareNumbers")

	    if g:atp_debugBS
		silent! echo "B l:list=" . string(l:list)
	    endif

	    let b:bibentryline=extend(b:bibentryline,{ l:f : l:list })

	    if g:atp_debugBS
		silent! echo "atplib#bibsearch b:bibentryline= (pattern != '') " . string(b:bibentryline)
	    endif

	endfor
    endif
"   CHECK EACH BIBFILE
    let l:bibresults={}
"     if the pattern was empty make it faster. 
    if a:pattern == ""
	for l:bibfile in keys(l:bibdict)
	    let l:bibfile_len=len(l:bibdict[l:bibfile])
	    let s:bibd={}
		let l:nr=0
		while l:nr < l:bibfile_len
		    let l:line=l:bibdict[l:bibfile][l:nr]
		    if l:line =~ pattern
			let s:lbibd={}
			let s:lbibd["bibfield_key"]=l:line
			let l:beg_line=l:nr+1
			let l:nr+=1
			let l:line=l:bibdict[l:bibfile][l:nr]
			let l:y=1
			while l:line !~ pattern && l:nr < l:bibfile_len
			    let l:line=l:bibdict[l:bibfile][l:nr]
			    let l:lkey=tolower(
					\ matchstr(
					    \ strpart(l:line,0,
						\ stridx(l:line,"=")
					    \ ),'\<\w*\>'
					\ ))
	" CONCATENATE LINES IF IT IS NOT ENDED
			    let l:y=1
			    if l:lkey != ""
				let s:lbibd[l:lkey]=l:line
	" IF THE LINE IS SPLIT ATTACH NEXT LINE									
				let l:nline=get(l:bibdict[l:bibfile],l:nr+l:y)
				while l:nline !~ '=' && 
					    \ l:nline !~ pattern &&
					    \ (l:nr+l:y) < l:bibfile_len
				    let s:lbibd[l:lkey]=substitute(s:lbibd[l:lkey],'\s*$','','') . " ". substitute(get(l:bibdict[l:bibfile],l:nr+l:y),'^\s*','','')
				    let l:line=get(l:bibdict[l:bibfile],l:nr+l:y)
				    let l:y+=1
				    let l:nline=get(l:bibdict[l:bibfile],l:nr+l:y)
				    if l:y > 30
					echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry (check line " . l:nr . ") in " . l:f . " line=".l:line
					break
				    endif
				endwhile
				if l:nline =~ pattern 
				    let l:y=1
				endif
			    endif
			    let l:nr+=l:y
			    unlet l:y
			endwhile
			let l:nr-=1
			call extend(s:bibd, { l:beg_line : s:lbibd })
		    else
			let l:nr+=1
		    endif
		endwhile
	    let l:bibresults[l:bibfile]=s:bibd
	    let g:bibresults=l:bibresults
	endfor
	let g:bibresults=l:bibresults

	if g:atp_debugBS
	    silent! echo "atplib#searchbib_bibresults A =" . l:bibresults
	endif

	return l:bibresults
    endif
    " END OF NEW CODE: (up)

    for l:bibfile in keys(b:bibentryline)
	let l:f=l:bibfile . ".bib"
"s:bibdict[l:f])	CHECK EVERY STARTING LINE (we are going to read bibfile from starting
"	line till the last matching } 
 	let s:bibd={}
 	for l:linenr in b:bibentryline[l:bibfile]

	    let l:nr=l:linenr-1
	    let l:i=atplib#count(get(l:bibdict[l:bibfile],l:linenr-1),"{")-atplib#count(get(l:bibdict[l:bibfile],l:linenr-1),"}")
	    let l:j=atplib#count(get(l:bibdict[l:bibfile],l:linenr-1),"(")-atplib#count(get(l:bibdict[l:bibfile],l:linenr-1),")") 
	    let s:lbibd={}
	    let s:lbibd["bibfield_key"]=get(l:bibdict[l:bibfile],l:linenr-1)
	    if s:lbibd["bibfield_key"] !~ '@\w\+\s*{.\+' 
		let l:l=0
		while get(l:bibdict[l:bibfile],l:linenr-l:l) =~ '^\s*$'
		    let l:l+=1
		endwhile
		let s:lbibd["bibfield_key"] .= get(l:bibdict[l:bibfile],l:linenr+l:l)
		let s:lbibd["bibfield_key"] = substitute(s:lbibd["bibfield_key"], '\s', '', 'g')
	    endif

	    let l:x=1
" we go from the first line of bibentry, i.e. @article{ or @article(, until the { and (
" will close. In each line we count brackets.	    
            while l:i>0	|| l:j>0
		let l:tlnr=l:x+l:linenr
		let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:tlnr-1),"{")
		let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:tlnr-1),"}")
		let l:i+=l:pos-l:neg
		let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:tlnr-1),"(")
		let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:tlnr-1),")")
		let l:j+=l:pos-l:neg
		let l:lkey=tolower(
			    \ matchstr(
				\ strpart(get(l:bibdict[l:bibfile],l:tlnr-1),0,
				    \ stridx(get(l:bibdict[l:bibfile],l:tlnr-1),"=")
				\ ),'\<\w*\>'
			    \ ))
		if l:lkey != ""
		    let s:lbibd[l:lkey]=get(l:bibdict[l:bibfile],l:tlnr-1)
			let l:y=0
" IF THE LINE IS SPLIT ATTACH NEXT LINE									
			if get(l:bibdict[l:bibfile],l:tlnr-1) !~ '\%()\|}\|"\)\s*,\s*\%(%.*\)\?$'
" 				    \ get(l:bibdict[l:bibfile],l:tlnr) !~ pattern_b
			    let l:lline=substitute(get(l:bibdict[l:bibfile],l:tlnr+l:y-1),'\\"\|\\{\|\\}\|\\(\|\\)','','g')
			    let l:pos=atplib#count(l:lline,"{")
			    let l:neg=atplib#count(l:lline,"}")
			    let l:m=l:pos-l:neg
			    let l:pos=atplib#count(l:lline,"(")
			    let l:neg=atplib#count(l:lline,")")
			    let l:n=l:pos-l:neg
			    let l:o=atplib#count(l:lline,"\"")
    " this checks if bracets {}, and () and "" appear in pairs in the current line:  
			    if l:m>0 || l:n>0 || l:o>l:o/2*2 
				while l:m>0 || l:n>0 || l:o>l:o/2*2 
				    let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"{")
				    let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"}")
				    let l:m+=l:pos-l:neg
				    let l:pos=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"(")
				    let l:neg=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),")")
				    let l:n+=l:pos-l:neg
				    let l:o+=atplib#count(get(l:bibdict[l:bibfile],l:tlnr+l:y),"\"")
    " Let's append the next line: 
				    let s:lbibd[l:lkey]=substitute(s:lbibd[l:lkey],'\s*$','','') . " ". substitute(get(l:bibdict[l:bibfile],l:tlnr+l:y),'^\s*','','')
				    let l:y+=1
				    if l:y > 30
					echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry at line " . l:linenr . " (check line " . l:tlnr . ") in " . l:f)
					break
				    endif
				endwhile
			    endif
			endif
		endif
" we have to go line by line and we could skip l:y+1 lines, but we have to
" keep l:m, l:o values. It do not saves much.		
		let l:x+=1
		if l:x > 30
			echoerr "ATP-Error /see :h atp-errors-bibsearch/, missing '}', ')' or '\"' in bibentry at line " . l:linenr . " in " . l:f
			break
	        endif
		let b:x=l:x
		unlet l:tlnr
	    endwhile
	    
	    let s:bibd[l:linenr]=s:lbibd
	    unlet s:lbibd
	endfor
	let l:bibresults[l:bibfile]=s:bibd
    endfor
    let g:bibresults=l:bibresults

    if g:atp_debugBS
	silent! echo "atplib#searchbib_bibresults A =" . string(l:bibresults)
	redir END
    endif

    return l:bibresults
endfunction
"}}}
" {{{ atplib#searchbib_py
function! atplib#searchbib_py(pattern, bibfiles, ...)
    call atplib#outdir()
    " for tex files this should be a flat search.
    let flat 	= &filetype == "plaintex" ? 1 : 0
    let bang	= a:0 >=1 ? a:1 : ""
    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)

    let b:atp_BibFiles=a:bibfiles
python << END
import vim, re

files=vim.eval("b:atp_BibFiles")

def remove_ligatures(string):
    line_without_ligatures = re.sub( "\\\\'\s*", '', re.sub('{|}|\\\\(?:"|`|\^|=|\.|c|~|v|u|d|b|H|t)\s*', '', string))
    line_without_ligatures = re.sub('\\\\oe', 'oe', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\OE', 'OE', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\ae', 'ae', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\AE', 'AE', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\o', 'o', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\O', 'O', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\i', 'i', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\j', 'j', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\l', 'l', line_without_ligatures)
    line_without_ligatures = re.sub('\\\\L', 'L', line_without_ligatures)
    return line_without_ligatures

def remove_quotes(string):
    line=re.sub("'", "\"", string)
    line=re.sub('\\\\', '', line)
    return line
type_pattern=re.compile('\s*@(article|book|mvbook|inbook|bookinbook|suppbook|booklet|collection|mvcollection|incollection|suppcollection|manual|misc|online|patent|periodical|supppertiodical|proceedings|mvproceedings|inproceedings|reference|mvreference|inreference|report|set|thesis|unpublished|custom[a-f]|conference|electronic|masterthesis|phdthesis|techreport|www)', re.I)

# types=['abstract', 'addendum', 'afterword', 'annotation', 'author', 'authortype', 'bookauthor', 'bookpaginator', 'booksupbtitle', 'booktitle', 'booktitleaddon', 'chapter', 'commentator', 'date', 'doi', 'edition', 'editor', 'editora', 'editorb', 'editorc', 'editortype', 'editoratype', 'editorbtype', 'editorctype', 'eid', 'eprint', 'eprintclass', 'eprinttype', 'eventdate', 'eventtile', 'file', 'forword', 'holder', 'howpublished', 'indxtitle', 'institution', 'introduction', 'isan', 'isbn', 'ismn', 'isrn', 'issn', 'issue', 'issuesubtitle', 'issuetitle', 'iswc', 'journalsubtitle', 'journaltitle', 'label', 'language', 'library', 'location', 'mainsubtitle', 'maintitle', 'maintitleaddon', 'month', 'nameaddon', 'note', 'number', 'organization', 'origdate', 'origlanguage', 'origpublisher', 'origname', 'pages', 'pagetotal', 'pagination', 'part', 'publisher', 'pubstate', 'reprinttitle', 'series', 'shortauthor', 'shorteditor', 'shorthand', 'shorthandintro', 'shortjournal', 'shortseries', 'subtitle', 'title', 'titleaddon', 'translator', 'type', 'url', 'urldate', 'venue', 'version', 'volume', 'volumes', 'year', 'crossref', 'entryset', 'entrysubtype', 'execute', 'mrreviewer']

types=['author', 'bookauthor', 'booktitle', 'date', 'editor', 'eprint', 'eprintclass', 'eprinttype', 'howpublished', 'institution', 'journal', 'month', 'note', 'number', 'organization', 'pages', 'publisher', 'school', 'series', 'subtitle', 'title', 'url', 'year', 'mrreviewer', 'volume', 'pages']

def parse_bibentry(bib_entry):
    bib={}
    bib['bibfield_key']=re.sub('\\r$', '', bib_entry[0])
    nr=1
    while nr < len(bib_entry)-1:
        line=bib_entry[nr]
        if not re.match('\s*%', line):
            if not re.search('=', line):
                while not re.search('=', line) and nr < len(bib_entry)-1:
                    val=re.sub('\s*$', '', bib[p_e_type])+" "+re.sub('^\s*', '', re.sub('\t', ' ', line))
                    val=re.sub('%.*', '', val)
                    bib[p_e_type]=remove_quotes(re.sub('\\r$', '', val))
                    nr+=1
                    line=bib_entry[nr]
            else:
                v_break=False
                for e_type in types:
                    if re.match('\s*'+e_type+'\s*=', line, re.I):
                        # this is not working when title is two lines!
                        line=re.sub('%.*', '', line)
                        bib[e_type]=remove_quotes(re.sub('\\r$', '', re.sub('\t', ' ', line)))
                        p_e_type=e_type
                        nr+=1
                        v_break=True
                        break
                if not v_break:
                    nr+=1
#    for key in bib.keys():
#        print(key+"="+bib[key])
#    print("\n")
    return bib

pattern=vim.eval("a:pattern")

if pattern == "":
    pat=""
else:
    pat=pattern
pattern=re.compile(pat, re.I)
pattern_b=re.compile('\s*@\w+\s*{.+', re.I)

bibresults={}
for file in files:
    file_ob=open(file, 'r')
    file_l=file_ob.read().split("\n")
    file_ob.close()
    file_len=len(file_l)
    lnr=0
    bibresults[file]={}
#     if pattern != ""
    while lnr < file_len:
        lnr+=1
        line=file_l[lnr-1]
	if re.search('@string', line):
            continue
        line_without_ligatures=remove_ligatures(line)
        if re.search(pattern, line_without_ligatures):
            """find first line"""
            b_lnr=lnr
#             print("lnr="+str(lnr))
            b_line=line
            while not re.match(pattern_b, b_line) and b_lnr >= 1:
                b_lnr-=1
                b_line=file_l[b_lnr-1]
            """find last line"""
#             print("b_lnr="+str(b_lnr))
            e_lnr=lnr
            e_line=line
            if re.match(pattern_b, e_line):
                lnr+=1
                e_lnr=lnr
                line=file_l[lnr-1]
                e_line=file_l[lnr-1]
#                 print("X "+line)
            while not re.match(pattern_b, e_line) and e_lnr <= file_len:
                e_lnr+=1
                e_line=file_l[min(e_lnr-1, file_len-1)]
            e_lnr-=1
            e_line=file_l[min(e_lnr-1, file_len-1)]
            while re.match('\s*$', e_line):
                e_lnr-=1
                e_line=file_l[e_lnr-1]
#             e_lnr=min(e_lnr, file_len-1)
            bib_entry=file_l[b_lnr-1:e_lnr]
#             print("lnr="+str(lnr))
#             print("b_lnr="+str(b_lnr))
#             print("e_lnr="+str(e_lnr))
            if bib_entry != [] and not re.search('@string', bib_entry[0]):
                entry_dict=parse_bibentry(bib_entry)
                bibresults[file][b_lnr]=entry_dict
#             else:
#                 print("lnr="+str(lnr))
#                 print("b_lnr="+str(b_lnr))
#                 print("e_lnr="+str(e_lnr))
#             print(entry_dict)
#             print("\n".join(bib_entry))
            if lnr < e_lnr:
                lnr=e_lnr
            else:
                lnr+=1
#print(bibresults)
# for key in bibresults.keys():
#     for line in bibresults[key].keys():
#         for bib in bibresults[key][line].keys():
#                 print(bib+"="+bibresults[key][line][bib])
#         print("\n")
vim.command("let bibresults="+str(bibresults))
END
let g:bibresults=bibresults
return bibresults
endfunction
"}}}
"
" {{{ atplib#SearchBibItems
" the argument should be b:atp_MainFile but in any case it is made in this way.
" it specifies in which file to search for include files.
function! atplib#SearchBibItems()
    let time=reltime()

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    " we are going to make a dictionary { citekey : label } (see :h \bibitem) 
    let l:citekey_label_dict={}

    " make a list of include files.
    let l:includefile_list=[]
    if !exists("b:ListOfFiles") || !exists("b:TypeDict")
	call TreeOfFiles(b:atp_MainFile)
    endif
    for f in b:ListOfFiles
	if b:TypeDict[f] == "input"
	    call add(l:includefile_list, f)
	endif
    endfor
    call add(l:includefile_list, atp_MainFile) 

    if has("python")
python << PEND
import vim, re
files=vim.eval("l:includefile_list")
citekey_label_dict={}
for f in files:
    f_o=open(f, 'r')
    f_l=f_o.read().split("\n")
    f_o.close()
    for line in f_l:
        if re.match('[^%]*\\\\bibitem', line):
            match=re.search('\\\\bibitem\s*(?:\[([^\]]*)\])?\s*{([^}]*)}\s*(.*)', line)
            if match:
                label=match.group(1)
                if label == None:
                    label = ""
                key=match.group(2)
                if key == None:
                    key = ""
                rest=match.group(3)
                if rest == None:
                    rest = ""
                if key != "":
                    citekey_label_dict[key]={ 'label' : label, 'rest' : rest }
vim.command("let l:citekey_label_dict="+str(citekey_label_dict))
PEND
    else
        " search for bibitems in all include files.
        for l:ifile in l:includefile_list

            let l:input_file = atplib#ReadInputFile(l:ifile,0)

                " search for bibitems and make a dictionary of labels and citekeys
                for l:line in l:input_file
                    if l:line =~# '^[^%]*\\bibitem'
                        let l:label=matchstr(l:line,'\\bibitem\s*\[\zs[^]]*\ze\]')
                        let l:key=matchstr(l:line,'\\bibitem\s*\%(\[[^]]*\]\)\?\s*{\zs[^}]*\ze}') 
                        let l:rest=matchstr(l:line,'\\bibitem\s*\%(\[[^]]*\]\)\?\s*{[^}]*}\s*\zs')
                        if l:key != ""
                            call extend(l:citekey_label_dict, { l:key : { 'label' : l:label, 'rest' : l:rest } }, 'error') 
                        endif
                    endif
                endfor
        endfor
    endif
	
    let g:time_SearchBibItems=reltimestr(reltime(time))
    return l:citekey_label_dict
endfunction
" }}}
" Showing results 
"{{{ atplib#showresults
" FLAGS:
" for currently supported flags see ':h atp_bibflags'
" All - all flags	
" L - last flag
" a - author
" e - editor
" t - title
" b - booktitle
" j - journal
" s - series
" y - year
" n - number
" v - volume
" p - pages
" P - publisher
" N - note
" S - school
" h - howpublished
" o - organization
" i - institution
" R - mrreviewer

function! atplib#showresults(bibresults, flags, pattern, bibdict)
 
    "if nothing was found inform the user and return:
    if len(a:bibresults) == count(a:bibresults, {})
	echo "BibSearch: no bib fields matched."
	if g:atp_debugBS
	    exe "redir! >> ".g:atp_TempDir."/BibSeach.log"
	    silent! echo "==========atplib#showresults================="
	    silent! echo "atplib#showresults return A - no bib fields matched. "
	    redir END
	endif
	return 0
    elseif g:atp_debugBS
	    exe "redir! >> ".g:atp_TempDir."/BibSearch.log"
	    silent! echo "==========atplib#showresults================="
	    silent! echo "atplib#showresults return B - found something. "
	    redir END
    endif

    function! s:showvalue(value)
	return substitute(strpart(a:value,stridx(a:value,"=")+1),'^\s*','','')
    endfunction

    let s:z=1
    let l:ln=1
    let l:listofkeys={}
"--------------SET UP FLAGS--------------------------    
	    let l:allflagon=0
	    let l:flagslist=[]
	    let l:kwflagslist=[]

    " flags o and i are synonims: (but refer to different entry keys): 
	if a:flags =~# 'i' && a:flags !~# 'o'
	    let l:flags=substitute(a:flags,'i','io','') 
	elseif a:flags !~# 'i' && a:flags =~# 'o'
	    let l:flags=substitute(a:flags,'o','oi','')
	endif
	if a:flags !~# 'All'
	    if a:flags =~# 'L'
"  		if strpart(a:flags,0,1) != '+'
"  		    let l:flags=b:atp_LastBibFlags . substitute(a:flags, 'L', '', 'g')
"  		else
 		    let l:flags=b:atp_LastBibFlags . substitute(a:flags, 'L', '', 'g')
"  		endif
		let g:atp_LastBibFlags = deepcopy(b:atp_LastBibFlags)
	    else
		if a:flags == "" 
		    let l:flags=g:defaultbibflags
		elseif strpart(a:flags,0,1) != '+' && a:flags !~ 'All' 
		    let l:flags=a:flags
		elseif strpart(a:flags,0,1) == '+' && a:flags !~ 'All'
		    let l:flags=g:defaultbibflags . strpart(a:flags,1)
		endif
	    endif
	    let b:atp_LastBibFlags=substitute(l:flags,'+\|L','','g')
		if l:flags != ""
		    let l:expr='\C[' . g:bibflagsstring . ']' 
		    while len(l:flags) >=1
			let l:oneflag=strpart(l:flags,0,1)
    " if we get a flag from the variable g:bibflagsstring we copy it to the list l:flagslist 
			if l:oneflag =~ l:expr
			    let l:flagslist=add(l:flagslist, l:oneflag)
			    let l:flags=strpart(l:flags,1)
    " if we get '@' we eat ;) two letters to the list l:kwflagslist			
			elseif l:oneflag == '@'
			    let l:oneflag=strpart(l:flags,0,2)
			    if index(keys(g:kwflagsdict),l:oneflag) != -1
				let l:kwflagslist=add(l:kwflagslist,l:oneflag)
			    endif
			    let l:flags=strpart(l:flags,2)
    " remove flags which are not defined
			elseif l:oneflag !~ l:expr && l:oneflag != '@'
			    let l:flags=strpart(l:flags,1)
			endif
		    endwhile
		endif
	else
    " if the flag 'All' was specified. 	    
	    let l:flagslist=split(g:defaultallbibflags, '\zs')
	    let l:af=substitute(a:flags,'All','','g')
	    for l:kwflag in keys(g:kwflagsdict)
		if a:flags =~ '\C' . l:kwflag	
		    call extend(l:kwflagslist,[l:kwflag])
		endif
	    endfor
	endif

	"NEW: if there are only keyword flags append default flags
	if len(l:kwflagslist) > 0 && len(l:flagslist) == 0 
	    let l:flagslist=split(g:defaultbibflags,'\zs')
	endif

"   Open a new window.
    let l:bufnr=bufnr("___Bibsearch: " . a:pattern . "___"  )
    if l:bufnr != -1
	let l:bdelete=l:bufnr . "bwipeout"
	exe l:bdelete
    endif
    unlet l:bufnr
    let l:openbuffer=" +setl\\ buftype=nofile\\ filetype=bibsearch_atp " . fnameescape("___Bibsearch: " . a:pattern . "___")
    if g:vertical ==1
	let l:openbuffer="keepalt vsplit " . l:openbuffer 
	let l:skip=""
    else
	let l:openbuffer="keepalt split " . l:openbuffer 
	let l:skip="       "
    endif

    let BufNr	= bufnr("%")
    let LineNr	= line(".")
    let ColNr	= col(".")
    silent exe l:openbuffer

"     set the window options
    silent call atplib#setwindow()
" make a dictionary of clear values, which we will fill with found entries. 	    
" the default value is no<keyname>, which after all is matched and not showed
" SPEED UP:
    let l:values={'bibfield_key' : 'nokey'}	
    for l:flag in g:bibflagslist
	let l:values_clear=extend(l:values,{ g:bibflagsdict[l:flag][0] : 'no' . g:bibflagsdict[l:flag][0] })
    endfor

" SPEED UP: 
    let l:kwflag_pattern="\\C"	
    let l:len_kwflgslist=len(l:kwflagslist)
    let l:kwflagslist_rev=reverse(deepcopy(l:kwflagslist))
    for l:lkwflag in l:kwflagslist
	if index(l:kwflagslist_rev,l:lkwflag) == 0 
	    let l:kwflag_pattern.=g:kwflagsdict[l:lkwflag]
	else
	    let l:kwflag_pattern.=g:kwflagsdict[l:lkwflag].'\|'
	endif
    endfor
"     let b:kwflag_pattern=l:kwflag_pattern

    for l:bibfile in keys(a:bibresults)
	if a:bibresults[l:bibfile] != {}
	    call setline(l:ln, "Found in " . l:bibfile )	
	    let l:ln+=1
	endif
	for l:linenr in copy(sort(keys(a:bibresults[l:bibfile]), "atplib#CompareNumbers"))
	    let l:values=deepcopy(l:values_clear)
	    let b:values=l:values
" fill l:values with a:bibrsults	    
	    let l:values["bibfield_key"]=a:bibresults[l:bibfile][l:linenr]["bibfield_key"]
" 	    for l:key in keys(l:values)
" 		if l:key != 'key' && get(a:bibresults[l:bibfile][l:linenr],l:key,"no" . l:key) != "no" . l:key
" 		    let l:values[l:key]=a:bibresults[l:bibfile][l:linenr][l:key]
" 		endif
" SPEED UP:
		call extend(l:values,a:bibresults[l:bibfile][l:linenr],'force')
" 	    endfor
" ----------------------------- SHOW ENTRIES -------------------------
" first we check the keyword flags, @a,@b,... it passes if at least one flag
" is matched
	    let l:check=0
" 	    for l:lkwflag in l:kwflagslist
" 	        let l:kwflagpattern= '\C' . g:kwflagsdict[l:lkwflag]
" 		if l:values['bibfield_key'] =~ l:kwflagpattern
" 		   let l:check=1
" 		endif
" 	    endfor
	    if l:values['bibfield_key'] =~ l:kwflag_pattern
		let l:check=1
	    endif
	    if l:check == 1 || len(l:kwflagslist) == 0
		let l:linenumber=index(a:bibdict[l:bibfile],l:values["bibfield_key"])+1
 		call setline(l:ln,s:z . ". line " . l:linenumber . "  " . l:values["bibfield_key"])
		let l:ln+=1
 		let l:c0=atplib#count(l:values["bibfield_key"],'{')-atplib#count(l:values["bibfield_key"],'(')

	
" this goes over the entry flags:
		for l:lflag in l:flagslist
" we check if the entry was present in bibfile:
		    if l:values[g:bibflagsdict[l:lflag][0]] != "no" . g:bibflagsdict[l:lflag][0]
" 			if l:values[g:bibflagsdict[l:lflag][0]] =~ a:pattern
			    call setline(l:ln, l:skip . g:bibflagsdict[l:lflag][1] . " = " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]]))
			    let l:ln+=1
" 			else
" 			    call setline(l:ln, l:skip . g:bibflagsdict[l:lflag][1] . " = " . s:showvalue(l:values[g:bibflagsdict[l:lflag][0]]))
" 			    let l:ln+=1
" 			endif
		    endif
		endfor
		let l:lastline=getline(line('$'))
		let l:c1=atplib#count(l:lastline,'{')-atplib#count(l:lastline,'}')
		let l:c2=atplib#count(l:lastline,'(')-atplib#count(l:lastline,')')
		let l:c3=atplib#count(l:lastline,'\"')
		if l:c0 == 1 && l:c1 == -1
		    call setline(line('$'),substitute(l:lastline,'}\s*$','',''))
		    call setline(l:ln,'}')
		    let l:ln+=1
		elseif l:c0 == 1 && l:c1 == 0	
		    call setline(l:ln,'}')
		    let l:ln+=1
		elseif l:c0 == -1 && l:c2 == -1
		    call setline(line('$'),substitute(l:lastline,')\s*$','',''))
		    call setline(l:ln,')')
		    let l:ln+=1
		elseif l:c0 == -1 && l:c1 == 0	
		    call setline(l:ln,')')
		    let l:ln+=1
		endif
		let l:listofkeys[s:z]=l:values["bibfield_key"]
		let s:z+=1
	    endif
	endfor
    endfor
    if g:atp_debugBS
	let g:pattern	= a:pattern
    endif
    if has("python") || g:atp_bibsearch == "python"
        let pattern_tomatch = substitute(a:pattern, '(', '\\(', 'g')
        let pattern_tomatch = substitute(pattern_tomatch, ')', '\\)', 'g')
        let pattern_tomatch = substitute(pattern_tomatch, '|', '\\|', 'g')
    else
        let pattern_tomatch = a:pattern
    endif
    let pattern_tomatch = substitute(pattern_tomatch, '\Co', 'oe\\=', 'g')
    let pattern_tomatch = substitute(pattern_tomatch, '\CO', 'OE\\=', 'g')
    let pattern_tomatch = substitute(pattern_tomatch, '\Ca', 'ae\\=', 'g')
    let pattern_tomatch = substitute(pattern_tomatch, '\CA', 'AE\\=', 'g')
    if g:atp_debugBS
	let g:pm = pattern_tomatch
    endif
    let pattern_tomatch	= join(split(pattern_tomatch, '\zs\\\@!\\\@<!'),  '[''"{\}]\{,3}')
    if g:atp_debugBS
	let g:pattern_tomatch = pattern_tomatch
    endif
    if pattern_tomatch != "" && pattern_tomatch != ".*"
	silent! call matchadd("Search", '\c' . pattern_tomatch)
	let @/=pattern_tomatch
    endif
    " return l:listofkeys which will be available in the bib search buffer
    " as b:ListOfKeys (see the BibSearch function below)
    let b:ListOfBibKeys = l:listofkeys
    let b:BufNr		= BufNr

    return l:listofkeys
endfunction
"}}}
"}}}
" URL query: (by some strange reason this is not working moved to url_query.py)
" function! atplib#URLquery(url) "{{{
" python << EOF
" import urllib2, tempfile, vim
" url  = vim.eval("a:url") 
" print(url)
" temp = tempfile.mkstemp("", "atp_ams_")
" 
" f    = open(temp[1], "w+")
" data = urllib2.urlopen(url)
" f.write(data.read())
" vim.command("return '"+temp[1]+"'")
" EOF
" endfunction "}}}

" This function sets the window options common for toc and bibsearch windows.
"{{{1 atplib#setwindow
" this function sets the options of BibSearch, ToC and Labels windows.
function! atplib#setwindow()
" These options are set in the command line
" +setl\\ buftype=nofile\\ filetype=bibsearch_atp   
" +setl\\ buftype=nofile\\ filetype=toc_atp\\ nowrap
" +setl\\ buftype=nofile\\ filetype=toc_atp\\ syntax=labels_atp
	setlocal nonumber
 	setlocal winfixwidth
	setlocal noswapfile	
	setlocal window
	setlocal nobuflisted
	if &filetype == "bibsearch_atp"
" 	    setlocal winwidth=30
	    setlocal nospell
	elseif &filetype == "toc_atp"
" 	    setlocal winwidth=20
	    setlocal nospell
	    setlocal cursorline 
	endif
" 	nnoremap <expr> <buffer> <C-W>l	"keepalt normal l"
" 	nnoremap <buffer> <C-W>h	"keepalt normal h"
endfunction
" }}}1
" {{{1 atplib#count
function! atplib#count(line,keyword,...)
   
    let method = ( a:0 == 0 || a:1 == 0 ) ? 0 : 1

    let line=a:line
    let i=0  
    if method==0
	while stridx(line, a:keyword) != '-1'
	    let line	= strpart(line, stridx(line, a:keyword)+1)
	    let i +=1
	endwhile
    elseif method==1
	let line=escape(line, '\\')
	while match(line, a:keyword . '\zs.*') != '-1'
	    let line=strpart(line, match(line, a:keyword . '\zs.*'))
	    let i+=1
	endwhile
    endif
    return i
endfunction
" }}}1
" Used to append / at the end of a directory name
" {{{1 atplib#append 	
fun! atplib#append(where, what)
    return substitute(a:where, a:what . '\s*$', '', '') . a:what
endfun
" }}}1
" Used to append extension to a filename (if there is no extension).
" {{{1 atplib#append_ext 
" extension has to be with a dot.
fun! atplib#append_ext(fname, ext)
    return substitute(a:fname, a:ext . '\s*$', '', '') . a:ext
endfun
" }}}1

" Check If Closed:
" This functions cheks if an environment is closed/opened.
" atplib#CheckClosed {{{1
" check if last bpat is closed.
" starting from the current line, limits the number of
" lines to search. It returns 0 if the environment is not closed or the line
" number where it is closed (an env is cannot be closed in 0 line)

" ToDo: the two function should only check not commented lines!
"
" Method 0 makes mistakes if the pattern is \begin:\end, if
" \begin{env_name}:\end{env_names} rather no (unless there are nested
" environments in the same name.
" Method 1 doesn't make mistakes and thus is preferable.
" after testing I shall remove method 0
function! atplib#CheckClosed(bpat, epat, line, limit,...)

"     NOTE: THIS IS MUCH FASTER !!! or SLOWER !!! ???            
"
"     let l:pos_saved=getpos(".") 
"     let l:cline=line(".")
"     if a:line != l:cline
" 	let l:col=len(getline(a:line))
" 	keepjumps call setpos(".",[0,a:line,l:col,0])
"     endif
"     let l:line=searchpair(a:bpat,'',a:epat,'nWr','',max([(a:line+a:limit),1]))
"     if a:line != l:cline
" 	keepjumps call setpos(".",l:pos_saved)
"     endif
"     return l:line


    if a:0 == 0 || a:1 == 0
	let l:method = 0
    else
	let l:method = a:1
    endif

    let l:len=len(getbufline(bufname("%"),1,'$'))
    let l:nr=a:line

    if a:limit == "$" || a:limit == "-1"
	let l:limit=l:len-a:line
    else
	let l:limit=a:limit
    endif

    if l:method==0
	while l:nr <= a:line+l:limit
	    let l:line=getline(l:nr)
	" Check if Closed
	    if l:nr == a:line
		if strpart(l:line,getpos(".")[2]-1) =~ '\%(' . a:bpat . '.*\)\@<!' . a:epat
		    return l:nr
		endif
	    else
		if l:line =~ '\%(' . a:epat . '.*\)\@<!' . a:bpat
		    return 0
		elseif l:line =~ '\%(' . a:bpat . '.*\)\@<!' . a:epat 
		    return l:nr
		endif
	    endif
	    let l:nr+=1
	endwhile

    elseif l:method==1

	let l:bpat_count=0
	let l:epat_count=0
	let l:begin_line=getline(a:line)
	let l:begin_line_nr=line(a:line)
	while l:nr <= a:line+l:limit
	    let l:line=getline(l:nr)
	" I assume that the env is opened in the line before!
	    let l:bpat_count+=atplib#count(l:line,a:bpat,1)
	    let l:epat_count+=atplib#count(l:line,a:epat,1)
	    if (l:bpat_count+1) == l:epat_count && l:begin_line !~ a:bpat
		return l:nr
	    elseif l:bpat_count == l:epat_count && l:begin_line =~ a:bpat
		return l:nr
	    endif 
	    let l:nr+=1
	endwhile
	return 0
    endif
endfunction
" }}}1
" atplib#CheckOpened {{{1
" Usage: By default (a:0 == 0 || a:1 == 0 ) it returns line number where the
" environment is opened if the environment is opened and is not closed (for
" completion), else it returns 0. However, if a:1 == 1 it returns line number
" where the environment is opened, if we are inside an environment (it is
" opened and closed below the starting line or not closed at all), it if a:1
" = 2, it just check if env is opened without looking if it is closed (
" cursor position is important).
" a:1 == 0 first non closed
" a:1 == 2 first non closed by counting.

" this function doesn't check if sth is opened in lines which begins with '\\def\>'
" (some times one wants to have a command which opens an environment.

" Todo: write a faster function using searchpairpos() which returns correct
" values.
function! atplib#CheckOpened(bpat,epat,line,limit,...)


"     this is almost good:    
"     let l:line=searchpair(a:bpat,'',a:epat,'bnWr','',max([(a:line-a:limit),1]))
"     return l:line

    if a:0 == 0 || a:1 == 0
	let l:check_mode = 0
    elseif a:1 == 1
	let l:check_mode = 1
    elseif a:1 == 2
	let l:check_mode = 2
    endif

    let l:len=len(getbufline(bufname("%"),1,'$'))
    let l:nr=a:line

    if a:limit == "^" || a:limit == "-1"
	let l:limit=a:line-1
    else
	let l:limit=a:limit
    endif

    if l:check_mode == 0 || l:check_mode == 1
	while l:nr >= a:line-l:limit && l:nr >= 1
	    let l:line=getline(l:nr)
		if l:nr == a:line
			if substitute(strpart(l:line,0,getpos(".")[2]), a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ a:bpat
			    return l:nr
			endif
		else
		    if l:check_mode == 0
			if substitute(l:line, a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ a:bpat
			    " check if it is closed up to the place where we start. (There
			    " is no need to check after, it will be checked anyway
			    " b a serrate call in TabCompletion.
			    if !atplib#CheckClosed(a:bpat,a:epat,l:nr,a:limit,0)
					    " LAST CHANGE 1->0 above
" 				let b:cifo_return=2 . " " . l:nr 
				return l:nr
			    endif
			endif
		    elseif l:check_mode == 1
			if substitute(l:line, a:bpat . '.\{-}' . a:epat,'','g')
				    \ =~ '\%(\\def\|\%(re\)\?newcommand\)\@<!' . a:bpat
			    let l:check=atplib#CheckClosed(a:bpat,a:epat,l:nr,a:limit,1)
			    " if env is not closed or is closed after a:line
			    if  l:check == 0 || l:check >= a:line
" 				let b:cifo_return=2 . " " . l:nr 
				return l:nr
			    endif
			endif
		    endif
		endif
	    let l:nr-=1
	endwhile
    elseif l:check_mode == 2
	let l:bpat_count=0
	let l:epat_count=0
	let l:begin_line=getline(".")
	let l:c=0
	while l:nr >= a:line-l:limit  && l:nr >= 1
	    let l:line=getline(l:nr)
	" I assume that the env is opened in line before!
" 		let l:line=strpart(l:line,getpos(".")[2])
	    let l:bpat_count+=atplib#count(l:line,a:bpat,1)
	    let l:epat_count+=atplib#count(l:line,a:epat,1)
	    if l:bpat_count == (l:epat_count+1+l:c) && l:begin_line != line(".") 
		let l:env_name=matchstr(getline(l:nr),'\\begin{\zs[^}]*\ze}')
		let l:check=atplib#CheckClosed('\\begin{' . l:env_name . '}', '\\end{' . l:env_name . '}',1,a:limit,1)
		if !l:check
		    return l:nr
		else
		    let l:c+=1
		endif
	    elseif l:bpat_count == l:epat_count && l:begin_line == line(".")
		return l:nr
	    endif 
	    let l:nr-=1
	endwhile
    endif
    return 0 
endfunction
" }}}1
" This functions makes a test if inline math is closed. This works well with
" \(:\) and \[:\] but not yet with $:$ and $$:$$.  
" {{{1 atplib#CheckInlineMath
" a:mathZone	= texMathZoneV or texMathZoneW or texMathZoneX or texMathZoneY
" The function return 1 if the mathZone is not closed 
function! atplib#CheckInlineMath(mathZone)
    let synstack	= map(synstack(line("."), max([1, col(".")-1])), "synIDattr( v:val, 'name')")
    let check		= 0
    let patterns 	= { 
		\ 'texMathZoneV' : [ '\\\@<!\\(', 	'\\\@<!\\)' 	], 
		\ 'texMathZoneW' : [ '\\\@<!\\\[', 	'\\\@<!\\\]'	]}
    " Limit the search to the first \par or a blank line, if not then search
    " until the end of document:
    let stop_line	= search('\\par\|^\s*$', 'nW') - 1
    let stop_line	= ( stop_line == -1 ? line('$') : stop_line )

    " \(:\), \[:\], $:$ and $$:$$ do not accept blank lines, thus we can limit
    " searching/counting.
    
    " For \(:\) and \[:\] we use searchpair function to test if it is closed or
    " not.
    if (a:mathZone == 'texMathZoneV' || a:mathZone == 'texMathZoneW') && atplib#CheckSyntaxGroups(['texMathZoneV', 'texMathZoneW'])
	if index(synstack, a:mathZone) != -1
	    let condition = searchpair( patterns[a:mathZone][0], '', patterns[a:mathZone][1], 'cnW', '', stop_line)
	    let check 	  = ( !condition ? 1 : check )
	endif

    " $:$ and $$:$$ we are counting $ untill blank line or \par
    " to test if it is closed or not, 
    " then we return the number of $ modulo 2.
    elseif ( a:mathZone == 'texMathZoneX' || a:mathZone == 'texMathZoneY' ) && atplib#CheckSyntaxGroups(['texMathZoneX', 'texMathZoneY'])
	let saved_pos	= getpos(".")
	let line	= line(".")	
	let l:count	= 0
	" count \$ if it is under the cursor
	if search('\\\@<!\$', 'Wc', stop_line)
	    let l:count += 1
	endif
	while line <= stop_line && line != 0
	    keepjumps let line	= search('\\\@<!\$', 'W', stop_line)
	    let l:count += 1
	endwhile
	keepjumps call setpos(".", saved_pos)
	let check	= l:count%2
    endif

    return check
endfunction
" {{{1 atplib#CheckSyntaxGroups
" This functions returns one if one of the environment given in the list
" a:zones is present in they syntax stack at line a:1 and column a:0.
" a:zones =	a list of zones
" a:1	  = 	line nr (default: current cursor line)
" a:2     =	column nr (default: column before the current cursor position)
" The function doesn't make any checks if the line and column supplied are
" valid /synstack() function returns 0 rather than [] in such a case/.
function! atplib#CheckSyntaxGroups(zones,...)
    let line		= a:0 >= 2 ? a:1 : line(".")
    let col		= a:0 >= 2 ? a:2 : col(".")-1
    let col		= max([1, col])
    let zones		= copy(a:zones)

    let synstack	= map(synstack( line, col), 'synIDattr(v:val, "name")') 

    return max(map(zones, "count(synstack, v:val)"))
endfunction
" atplib#CopyIndentation {{{1
function! atplib#CopyIndentation(line)
    let raw_indent	= split(a:line,'\s\zs')
    let indent		= ""
    for char in raw_indent
	if char =~ '^\%(\s\|\t\)'
	    let indent.=char
	else
	    break
	endif
    endfor
    return indent
endfunction
"}}}1

" Tab Completion Related Functions:
" atplib#SearchPackage {{{1
"
" This function searches if the package in question is declared or not.
" Returns the line number of the declaration  or 0 if it was not found.
"
" It was inspired by autex function written by Carl Mueller, math at carlm e4ward c o m
" and made work for project files using lvimgrep.
"
" This function doesn't support plaintex files (\\input{})
" ATP support plaintex input lines in a different way (but not that flexible
" as this: for plaintex I use atplib#GrepPackageList on startup (only!) and
" then match input name with the list).
"
" name = package name (tikz library name)
" a:1  = stop line (number of the line \\begin{document} 
" a:2  = pattern matching the command (without '^[^%]*\\', just the name)
" to match \usetikzlibrary{...,..., - 
function! atplib#SearchPackage(name,...)

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    if !filereadable(atp_MainFile)
	silent echomsg "[ATP:] atp_MainFile : " . atp_MainFile . " is not readable "
	return
    endif
    let cwd = getcwd()
    if exists("b:atp_ProjectDir") && getcwd() != b:atp_ProjectDir
	exe "lcd " . fnameescape(b:atp_ProjectDir)
    endif

    if getbufvar("%", "atp_MainFile") == ""
	call SetProjectName()
    endif

"     let time	= reltime()

"     if bufloaded("^" . a:file . "$")
" 	let file=getbufline("^" . a:file . "$", "1", "$")
"     else
" 	let file=readfile(a:filename)
"     endif

    if a:0 != 0
	let stop_line	= a:1
    else
	if expand("%:p") == atp_MainFile
	    let saved_pos=getpos(".")
	    keepjumps call setpos(".", [0,1,1,0])
	    keepjumps let stop_line=search('\\begin\s*{\s*document\s*}','nW')
	else
	    if &l:filetype == 'tex'
		let saved_loclist	= getloclist(0)
		silent! execute '1lvimgrep /\\begin\s*{\s*document\s*}/j ' . fnameescape(atp_MainFile)
		let stop_line	= get(get(getloclist(0), 0, {}), 'lnum', 0)
		call setloclist(0, saved_loclist) 
	    else
		let stop_line = 0
	    endif
	endif
    endif

    let com	= a:0 >= 2 ? a:2 : 'usepackage\s*\%(\[[^]]*]\)\?'

    " If the current file is the atp_MainFile
    if expand("%:p") == atp_MainFile

	if !exists("saved_pos")
	    let saved_pos=getpos(".")
	endif
	if stop_line != 0

	    keepjumps call setpos(".",[0,1,1,0])
	    keepjumps let ret = search('\C^[^%]*\\'.com."\s*{[^}]*".a:name,'ncW', stop_line)
	    keepjump call setpos(".",saved_pos)

	    exe "lcd " . fnameescape(cwd)
	    return ret

	else

	    keepjumps call setpos(".",[0,1,1,0])
	    keepjumps let ret = search('\C^[^%]*\\'.com."\s*{[^}]*".a:name,'ncW')
	    keepjump call setpos(".", saved_pos)

	    exe "lcd " . fnameescape(cwd)
	    return ret

	endif

    " If the current file is not the mainfile
    else
	" Cache the Preambule / it is not changing so this is completely safe /
	if !exists("s:Preambule")
	    let s:Preambule = readfile(atp_MainFile) 
	    if stop_line != 0
		silent! call remove(s:Preambule, stop_line+1, -1)
	    endif
	endif
	let lnum = 1
	for line in s:Preambule
	    if line =~ '^[^%]*\\'.com."\s*{[^}]*\C".a:name

" 		echo reltimestr(reltime(time))
		exe "lcd " . fnameescape(cwd)
		return lnum
	    endif
	    let lnum += 1
	endfor
    endif

"     echo reltimestr(reltime(time))

    " If the package was not found return 0 
    exe "lcd " . fnameescape(cwd)
    return 0

endfunction
" }}}1
"{{{1 atplib#GrepPackageList
" This function returns list of packages declared in the b:atp_MainFile (or
" a:2). If the filetype is plaintex it returns list of all \input{} files in
" the b:atp_MainFile. 
" I'm not shure if this will be OK for project files written in plaintex: Can
" one declare a package in the middle of document? probably yes. So it might
" be better to use TreeOfFiles in that case.

" This takes =~ 0.02 s. This is too long to call it in TabCompletion.
function! atplib#GrepPackageList(...)
" 	let time = reltime() 
    let file	= a:0 >= 2 ? a:2 : getbufvar("%", "atp_MainFile") 
    let pat	= a:0 >= 1 ? a:1 : ''
    if file == ""
	return []
    endif

    let ftype	= getbufvar(file, "&filetype")
    if pat == ''
	if ftype =~ '^\(ams\)\=tex$'
	    let pat	= '\\usepackage\s*\(\[[^]]*\]\)\=\s*{'
	elseif ftype == 'plaintex'
	    let pat = '\\input\s*{'
	else
    " 	echoerr "ATP doesn't recognize the filetype " . &l:filetype . ". Using empty list of packages."
	    return []
	endif
    endif

    let saved_loclist	= getloclist(0)
    try
	silent execute 'lvimgrep /^[^%]*'.pat.'/j ' . fnameescape(file)
    catch /E480:/
	call setloclist(0, [{'text' : 'empty' }])
    endtry
    let loclist		= getloclist(0)
    call setloclist(0, saved_loclist)

    let pre		= map(loclist, 'v:val["text"]')
    let pre_l		= []
    for line in pre
	let package_l	= matchstr(line, pat.'\zs[^}]*\ze}')
	call add(pre_l, package_l)
    endfor

    " We make a string of packages separeted by commas and the split it
    " (compatibility with \usepackage{package_1,package_2,...})
    let pre_string	= join(pre_l, ',')
    let pre_list	= split(pre_string, ',')
    call filter(pre_list, "v:val !~ '^\s*$'")

"      echo reltimestr(reltime(time))
    return pre_list
endfunction
"{{{1 atplib#GrepPreambule
function! atplib#GrepPreambule(pattern, ...)
    let saved_loclist 	= getloclist(0)
    let atp_MainFile	= ( a:0 >= 1 ? a:1 : b:atp_MainFile ) 
    let winview = winsaveview()
    exe 'silent! 1lvimgrep /^[^%]*\\begin\s*{\s*document\s*}/j ' . fnameescape(atp_MainFile)
    let linenr = get(get(getloclist(0), 0, {}), 'lnum', 'nomatch')
    if linenr == "nomatch"
	call setloclist(0, saved_loclist)
	return
    endif
    exe 'silent! lvimgrep /'.a:pattern.'\%<'.linenr.'l/jg ' . fnameescape(atp_MainFile) 
    let matches = getloclist(0)
    call setloclist(0, saved_loclist)
    return matches
endfunction

" atplib#DocumentClass {{{1
function! atplib#DocumentClass(file)

    let saved_loclist	= getloclist(0)
    try
	silent execute 'lvimgrep /\\documentclass/j ' . fnameescape(a:file)
    catch /E480:/
    endtry
    let line		= get(get(getloclist(0), 0, { 'text' : "no_document_class"}), 'text')
    call setloclist(0, saved_loclist)


    if line != 'no_document_class'
	return substitute(l:line,'.*\\documentclass\s*\%(\[.*\]\)\?{\(.*\)}.*','\1','')
    endif
 
    return 0
endfunction
" }}}1

" Searching Tools: (kpsewhich)
" {{{1 atplib#KpsewhichGlobPath 
" 	a:format	is the format as reported by kpsewhich --help
" 	a:path		path if set to "", then kpse which will find the path.
" 			The default is what 'kpsewhich -show-path tex' returns
" 			with "**" appended. 
" 	a:name 		can be "*" then finds all files with the given extension
" 			or "*.cls" to find all files with a given extension.
" 	a:1		modifiers (the default is ":t:r")
" 	a:2		filters path names matching the pattern a:1
" 	a:3		filters out path names not matching the pattern a:2
"
" 	Argument a:path was added because it takes time for kpsewhich to return the
" 	path (usually ~0.5sec). ATP asks kpsewhich on start up
" 	(g:atp_kpsewhich_tex) and then locks the variable (this will work
" 	unless sb is reinstalling tex (with different personal settings,
" 	changing $LOCALTEXMF) during vim session - not that often). 
"
" Example: call atplib#KpsewhichGlobPath('tex', '', '*', ':p', '^\(\/home\|\.\)','\%(texlive\|kpsewhich\|generic\)')
" gives on my system only the path of current dir (/.) and my localtexmf. 
" this is done in 0.13s. The long pattern is to 
"
" atp#KpsewhichGlobPath({format}, {path}, {expr=name}, [ {mods}, {pattern_1}, {pattern_2}]) 
function! atplib#KpsewhichGlobPath(format, path, name, ...)
"     let time	= reltime()
    let modifiers = a:0 == 0 ? ":t:r" : a:1
    if a:path == ""
	let path	= substitute(substitute(system("kpsewhich -show-path ".a:format ),'!!','','g'),'\/\/\+','\/','g')
	let path	= substitute(path,':\|\n',',','g')
	let path_list	= split(path, ',')
	let idx		= index(path_list, '.')
	if idx != -1
	    let dot 	= remove(path_list, index(path_list,'.')) . ","
	else
	    let dot 	= ""
	endif
	call map(path_list, 'v:val . "**"')

	let path	= dot . join(path_list, ',')
    else
	let path = a:path
    endif
    " If a:2 is non zero (if not given it is assumed to be 0 for compatibility
    " reasons)
    if get(a:000, 1, 0) != "0"
	let path_list	= split(path, ',')
	call filter(path_list, 'v:val =~ a:2')
	let path	= join(path_list, ',')
    endif
    if get(a:000, 2, 0) != "0"
	let path_list	= split(path, ',')
	call filter(path_list, 'v:val !~ a:3')
	let path	= join(path_list, ',')
    endif

    let list	= split(globpath(path, a:name),'\n') 
    call map(list, 'fnamemodify(v:val, modifiers)')
    return list
endfunction
" }}}1
" {{{1 atplib#KpsewhichFindFile
" the arguments are similar to atplib#KpsewhichGlob except that the a:000 list
" is shifted:
" a:1		= path	
" 			if set to "" then kpsewhich will find the path.
" a:2		= count (as for findfile())
" a:3		= modifiers 
" a:4		= positive filter for path (see KpsewhichGLob a:1)
" a:5		= negative filter for path (see KpsewhichFind a:2)
"
" needs +path_extra vim feature
"
" atp#KpsewhichFindFile({format}, {expr=name}, [{path}, {count}, {mods}, {pattern_1}, {pattern_2}]) 
function! atplib#KpsewhichFindFile(format, name, ...)

    " Unset the suffixadd option
    let saved_sua	= &l:suffixesadd
    let &l:sua	= ""

"     let time	= reltime()
    let path	= a:0 >= 1 ? a:1 : ""
    let l:count	= a:0 >= 2 ? a:2 : 0
    let modifiers = a:0 >= 3 ? a:3 : ""
    " This takes most of the time!
    if path == ""
	let path	= substitute(substitute(system("kpsewhich -show-path ".a:format ),'!!','','g'),'\/\/\+','\/','g')
	let path	= substitute(path,':\|\n',',','g')
	let path_list	= split(path, ',')
	let idx		= index(path_list, '.')
	if idx != -1
	    let dot 	= remove(path_list, index(path_list,'.')) . ","
	else
	    let dot 	= ""
	endif
	call map(path_list, 'v:val . "**"')

	let path	= dot . join(path_list, ',')
	unlet path_list
    endif


    " If a:2 is non zero (if not given it is assumed to be 0 for compatibility
    " reasons)
    if get(a:000, 3, 0) != 0
	let path_list	= split(path, ',')
	call filter(path_list, 'v:val =~ a:4')
	let path	= join(path_list, ',')
    endif
    if get(a:000, 4, 0) != 0
	let path_list	= split(path, ',')
	call filter(path_list, 'v:val !~ a:5')
	let path	= join(path_list, ',')
    endif

    if l:count >= 1
	let result	= findfile(a:name, path, l:count)
    elseif l:count == 0
	let result	= findfile(a:name, path)
    elseif l:count < 0
	let result	= findfile(a:name, path, -1)
    endif
	

    if l:count >= 0 && modifiers != ""
	let result	= fnamemodify(result, modifiers) 
    elseif l:count < 0 && modifiers != ""
	call map(result, 'fnamemodify(v:val, modifiers)')
    endif

    let &l:sua	= saved_sua
    return result
endfunction
" }}}1

" List Functions:
" atplib#Extend {{{1
" arguments are the same as for extend(), but it adds only the entries which
" are not present.
function! atplib#Extend(list_a,list_b,...)
    let list_a=deepcopy(a:list_a)
    let list_b=deepcopy(a:list_b)
    let diff=filter(list_b,'count(l:list_a,v:val) == 0')
    if a:0 == 0
	return extend(list_a,diff)
    else
	return extend(list_a,diff, a:1)
    endif
endfunction
" }}}1
" {{{1 atplib#Add
function! atplib#Add(list,what)
    let new=[] 
    for element in a:list
	call add(new,element . a:what)
    endfor
    return new
endfunction
"}}}1

" Close Environments And Brackets:
" Close Last Environment
" atplib#CloseLastEnvironment {{{1
" a:1 = i	(append before, so the cursor will  be after - the dafult)  
" 	a	(append after)
" a:2 = math 		the pairs \(:\), $:$, \[:\] or $$:$$ (if filetype is
" 						plaintex or b:atp_TexFlavor="plaintex")
" 	environment
" 			by the way, treating the math pairs together is very fast. 
" a:3 = environment name (if present and non zero sets a:2 = environment)	
" 	if one wants to find an environment name it must be 0 or "" or not
" 	given at all.
" a:4 = line and column number (in a vim list) where environment is opened
" ToDo: Ad a highlight to messages!!! AND MAKE IT NOT DISAPPEAR SOME HOW?
" (redrawing doesn't help) CHECK lazyredraw. 
" Note: this function tries to not double the checks what to close if it is
" given in the arguments, and find only the information that is not given
" (possibly all the information as all arguments can be omitted).
function! atplib#CloseLastEnvironment(...)

    let l:com	= a:0 >= 1 ? a:1 : 'i'
    let l:close = a:0 >= 2 && a:2 != "" ? a:2 : 0
    if a:0 >= 3
	let l:env_name	= a:3 == "" ? 0 : a:3
	let l:close 	= ( a:3 != '' ? "environment" : l:close )
    else
	let l:env_name 	= 0
    endif
    let l:bpos_env	= a:0 >= 4 ? a:4 : [0, 0]

    if g:atp_debugCloseLastEnvironment
	exe "redir! > " . g:atp_TempDir."/CloseLastEnvironment.log"
	let g:CLEargs 	= l:com . " " . l:close . " " . l:env_name . " " . string(l:bpos_env)
	silent echo "args=".g:CLEargs
	let g:com	= l:com
	let g:close 	= l:close
	let g:env_name	= l:env_name
	let g:bpos_env	= l:bpos_env
    endif

"   {{{2 find the begining line of environment to close (if we are closing
"   an environment)
    if l:env_name == 0 && ( l:close == "environment" || l:close == 0 ) && l:close != "math"

	let filter 	= 'strpart(getline(''.''), 0, col(''.'') - 1) =~ ''\\\@<!%'''

	" Check if and environment is opened (\begin:\end):
	" This is the slow part :( 0.4s)
	" Find the begining line if it was not given.
	if l:bpos_env == [0, 0]
	    " Find line where the environment is opened and not closed:
	    let l:bpos_env		= searchpairpos('\\begin\s*{', '', '\\end\s*{', 'bnW', 'searchpair("\\\\begin\s*{\s*".matchstr(getline("."),"\\\\begin\s*{\\zs[^}]*\\ze\}"), "", "\\\\end\s*{\s*".matchstr(getline("."), "\\\\begin\s*{\\zs[^}]*\\ze}"), "nW", "", "line(".")+g:atp_completion_limits[2]")',max([ 1, (line(".")-g:atp_completion_limits[2])]))
	endif

	let l:env_name		= matchstr(strpart(getline(l:bpos_env[0]),l:bpos_env[1]-1), '\\begin\s*{\s*\zs[^}]*\ze*\s*}')

    " if a:3 (environment name) was given:
    elseif l:env_name != "0" && l:close == "environment" 

	let l:bpos_env	= searchpairpos('\\begin\s*{'.l:env_name.'}', '', '\\end\s*{'.l:env_name.'}', 'bnW', '',max([1,(line(".")-g:atp_completion_limits[2])]))

    endif
"   }}}2
"   {{{2 if a:2 was not given (l:close = 0) we have to check math modes as
"   well.
    if ( l:close == "0" || l:close == "math" ) && l:bpos_env == [0, 0] 

	let stopline 		= search('^\s*$\|\\par\>', 'bnW')

	" Check if one of \(:\), \[:\], $:$, $$:$$ is opened using syntax
	" file. If it is fined the starting position.

	let synstack		= map(synstack(line("."),max([1, col(".")-1])), 'synIDattr(v:val, "name")')
	if g:atp_debugCloseLastEnvironment
	    let g:synstackCLE	= deepcopy(synstack)
	    let g:openCLE	= getline(".")[col(".")-1] . getline(".")[col(".")]
	    silent echo "synstack=".string(synstack)
	    silent echo "g:openCLE=".string(g:openCLE)
	endif
	let bound_1		= getline(".")[col(".")-1] . getline(".")[col(".")] =~ '^\\\%((\|)\)$'
	let math_1		= (index(synstack, 'texMathZoneV') != -1 && !bound_1 ? 1  : 0 )   
	    if math_1
		if l:bpos_env == [0, 0]
		    let bpos_math_1	= searchpos('\%(\%(\\\)\@<!\\\)\@<!\\(', 'bnW', stopline)
		else
		    let bpos_math_1	= l:bpos_env
		endif
		let l:begin_line= bpos_math_1[0]
		let math_mode	= "texMathZoneV"
	    endif
	" the \[:\] pair:
	let bound_2		= getline(".")[col(".")-1] . getline(".")[col(".")] =~ '^\\\%(\[\|\]\)$'
	let math_2		= (index(synstack, 'texMathZoneW') != -1 && !bound_2 ? 1  : 0 )   
	    if math_2
		if l:bpos_env == [0, 0]
		    let bpos_math_2	= searchpos('\%(\%(\\\)\@<!\\\)\@<!\\[', 'bnW', stopline)
		else
		    let bpos_math_2	= l:bpos_env
		endif
		let l:begin_line= bpos_math_2[0]
		let math_mode	= "texMathZoneW"
	    endif
	" the $:$ pair:
	let bound_3		= getline(".")[col(".")-1] =~ '^\$$'
	let math_3		= (index(synstack, 'texMathZoneX') != -1 && !bound_3 ? 1  : 0 )   
	    if math_3
		if l:bpos_env == [0, 0]
		    let bpos_math_3	= searchpos('\%(\%(\\\)\@<!\\\)\@<!\$\{1,1}', 'bnW', stopline)
		else
		    let bpos_math_3	= l:bpos_env
		endif
		let l:begin_line= bpos_math_3[0]
		let math_mode	= "texMathZoneX"
	    endif
	" the $$:$$ pair:
	let bound_4		= getline(".")[col(".")-1] . getline(".")[col(".")] =~ '^\$\$$'
	let math_4		= (index(synstack, 'texMathZoneY') != -1 && !bound_4 ? 1  : 0 )   
	    if math_4
		if l:bpos_env == [0, 0]
		    let bpos_math_4	= searchpos('\%(\%(\\\)\@<!\\\)\@<!\$\{2,2}', 'bnW', stopline)
		else
		    let bpos_math_4	= l:bpos_env
		endif
		let l:begin_line= bpos_math_4[0]
		let math_mode	= "texMathZoneY"
	    endif
	if g:atp_debugCloseLastEnvironment
	    let g:math 	= []
	    let g:bound = []
	    for i in [1,2,3,4]
		let g:begin_line = ( exists("begin_line") ? begin_line : 0 )
		let g:bound_{i} = bound_{i}
		call add(g:bound, bound_{i})
		let g:math_{i} = math_{i}
		call add(g:math, math_{i})
	    endfor
	    silent echo "g:begin_line=".g:begin_line
	    silent echo "g:bound=".string(g:bound)
	    silent echo "g:math=".string(g:math)
	    silent echo "math_mode=".( exists("math_mode") ? math_mode : "None" )
	endif
    elseif ( l:close == "0" || l:close == "math" )
	let string = getline(l:bpos_env[0])[l:bpos_env[1]-2] . getline(l:bpos_env[0])[l:bpos_env[1]-1] . getline(l:bpos_env[0])[l:bpos_env[1]]
	let stop_line = search('\\par\|^\s*$\|\\\%(begin\|end\)\s*{', 'n')
	let [ math_1, math_2, math_3, math_4 ] = [ 0, 0, 0, 0 ]
	let saved_pos		= getpos(".")
	if string =~ '\\\@<!\\('
	    call cursor(l:bpos_env) 
	    " Check if closed:
	    let math_1 		= searchpair('\\(', '', '\\)', 'n', '', stop_line)
	    if !math_1
		let math_mode	= "texMathZoneV"
	    endif
	elseif string =~ '\\\@<!\\\['
	    call cursor(l:bpos_env) 
	    " Check if closed:
	    let math_2 		= searchpair('\\\[', '', '\\\]', 'n', '', stop_line)
	    if !math_2
		let math_mode	= "texMathZoneW"
	    endif
	elseif string =~ '\%(\\\|\$\)\@<!\$\$\@!'
	    " Check if closed: 	not implemented
	    let math_3 		= 0
	    let math_mode	= "texMathZoneX"
	elseif string =~ '\\\@<!\$\$'
	    " Check if closed: 	not implemented
	    let math_4 		= 0
	    let math_mode	= "texMathZoneY"
	endif
	call cursor([ saved_pos[1], saved_pos[2] ]) 
	if g:atp_debugCloseLastEnvironment
	    if exists("math_mode")
		let g:math_mode  	= math_mode
		silent echo "math_mode=".math_mode
	    endif
	    let g:math 	= []
	    let g:string = string
	    for i in [1,2,3,4]
		let g:begin_line = ( exists("begin_line") ? begin_line : 0 )
		let g:math_{i} = math_{i}
		call add(g:math, math_{i})
	    endfor
	    silent echo "g:begin_line".g:begin_line
	    silent echo "g:math=".string(g:math)
	endif
	if exists("math_mode")
	    let l:begin_line 	= l:bpos_env[0]
	    if g:atp_debugCloseLastEnvironment
		silent echo "math_mode=".math_mode
		silent echo "l:begin_line=".l:begin_line
		redir END
	    endif
	else
	    if g:atp_debugCloseLastEnvironment
		silent echo "Given coordinates are closed."
		redir END
	    endif
	    return " Given coordinates are closed."
	endif
    endif
"}}}2
"{{{2 set l:close if a:1 was not given.
if a:0 <= 1
" 	let l:begin_line=max([ l:begin_line_env, l:begin_line_imath, l:begin_line_dmath ])
    " now we can set what to close:
    " the synstack never contains two of the math zones: texMathZoneV,
    " texMathZoneW, texMathZoneX, texMathZoneY.
    if math_1 + math_2 + math_3 + math_4 >= 1
	let l:close = 'math'
    elseif l:env_name
	let l:close = 'environment'
    else
	if g:atp_debugCloseLastEnvironment
	    silent echo "return: l:env_name=".string(l:env_name)." && math_1+...+math_4=".string(math_1+math_2+math_3+math_4)
	    redir END
	endif
	return
    endif
endif
if g:atp_debugCloseLastEnvironment
    let g:close = l:close
    silent echo "g:close=".string(l:close)
    silent echo "l:env_name=".l:env_name
endif
let l:env=l:env_name
"}}}2

if l:close == "0" || l:close == 'math' && !exists("begin_line")
    if g:atp_debugCloseLastEnvironment
	silent echo "there was nothing to close"
	redir END
    endif
    return "there was nothing to close"
endif
if ( &filetype != "plaintex" && b:atp_TexFlavor != "plaintex" && exists("math_4") && math_4 )
    echohl ErrorMsg
    echomsg "[ATP:] $$:$$ in LaTeX are deprecated (this breaks some LaTeX packages)" 
    echomsg "       You can set b:atp_TexFlavor = 'plaintex', and ATP will ignore this. "
    echohl Normal
    if g:atp_debugCloseLastEnvironment
	silent echo "return A"
	redir END
    endif
    return 
endif
if l:env_name =~ '^\s*document\s*$'
    if g:atp_debugCloseLastEnvironment
	silent echo "return B"
	redir END
    endif
    return ""
endif
let l:cline	= getline(".")
let l:pos	= getpos(".")
if l:close == "math"
    let l:line	= getline(l:begin_line)
elseif l:close == "environment"
    let l:line	= getline(l:bpos_env[0])
endif

    if g:atp_debugCloseLastEnvironment
	let g:line = exists("l:line") ? l:line : 0
	silent echo "g:line=".g:line
    endif

" Copy the indentation of what we are closing.
let l:eindent=atplib#CopyIndentation(l:line)
"{{{2 close environment
    if l:close == 'environment'
	" Info message
	redraw
" 	silent echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0]

	" Rules:
	" env & \[ \]: close in the same line 
	" unless it starts in a serrate line,
	" \( \): close in the same line. 
	"{{{3 close environment in the same line
	if l:line !~ '^\s*\%(\$\|\$\$\|[^\\]\\%(\|\\\@<!\\\[\)\?\s*\\begin\s*{[^}]*}\s*\%((.*)\s*\|{.*}\s*\|\[.*\]\s*\)\{,3}\%(\s*\\label\s*{[^}]*}\s*\|\s*\\hypertarget\s*{[^}]*}\s*{[^}]*}\s*\)\{0,2}$'
	    " I use \[.*\] instead of \[[^\]*\] which doesn't work with nested
	    " \[:\] the same for {:} and (:).
" 	    	This pattern matches:
" 	    		^ $
" 	    		^ $$
" 	    		^ \(
" 	    		^ \[
" 	    		^ (one of above or space) \begin { env_name } ( args1 ) [ args2 ] { args3 } \label {label} \hypertarget {} {}
" 	    		There are at most 3 args of any type with any order \label and \hypertarget are matched optionaly.
" 	    		Any of these have to be followd by white space up to end of line.
	    "
	    " The line above cannot contain "\|^\s*$" pattern! Then the
	    " algorithm for placing the \end in right place is not called.
	    "
	    " 		    THIS WILL BE NEEDED LATER!
" 		    \ (l:close == 'display_math' 	&& l:line !~ '^\s*[^\\]\\\[\s*$') ||
" 		    \ (l:close == 'inline_math' 	&& (l:line !~ '^\s*[^\\]\\(\s*$' || l:begin_line == line("."))) ||
" 		    \ (l:close == 'dolar_math' 		&& l:cline =~ '\$')

	    " the above condition matches for the situations when we have to
	    " complete in the same line in four cases:
	    " l:close == environment, display_math, inline_math or
	    " dolar_math. 

	    " do not complete environments which starts in a definition.
" let b:cle_debug= (getline(l:begin_line) =~ '\\def\|\%(re\)\?newcommand') . " " . (l:begin_line != line("."))
" 	    if getline(l:begin_line) =~ '\\def\|\%(re\)\?newcommand' && l:begin_line != line(".")
"  		let b:cle_return="def"
" 		return b:cle_return
" 	    endif
	    if index(g:atp_no_complete, l:env) == '-1' &&
		\ !atplib#CheckClosed('\%(%.*\)\@<!\\begin\s*{' . l:env,'\%(%.*\)\@<!\\end\s*{' . l:env,line("."),g:atp_completion_limits[2])
		if l:com == 'a'  
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\end{'.l:env.'}' . strpart(l:cline,getpos(".")[2]))
		    let l:pos=getpos(".")
		    let l:pos[2]=len(strpart(l:cline,0,getpos(".")[2]) . '\end{'.l:env.'}')+1
		    keepjumps call setpos(".",l:pos)
		elseif l:cline =~ '^\s*$'
		    call setline(line("."), l:eindent . '\end{'.l:env.'}' . strpart(l:cline,getpos(".")[2]-1))
		    let l:pos=getpos(".")
		    let l:pos[2]=len(strpart(l:cline,0,getpos(".")[2]-1) . '\end{'.l:env.'}')+1
		    keepjumps call setpos(".",l:pos)
		else
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\end{'.l:env.'}' . strpart(l:cline,getpos(".")[2]-1))
		    let l:pos=getpos(".")
		    let l:pos[2]=len(strpart(l:cline,0,getpos(".")[2]-1) . '\end{'.l:env.'}')+1
		    keepjumps call setpos(".",l:pos)
		endif
	    endif "}}}3
	"{{{3 close environment in a new line 
	else 

		" do not complete environments which starts in a definition.

		let l:error=0
		let l:prev_line_nr="-1"
		let l:cenv_lines=[]
		let l:nr=line(".")
		
		let l:line_nr=line(".")
		" l:line_nr number of line which we complete
		" l:cenv_lines list of closed environments (we complete after
		" line number maximum of these numbers.

		let l:pos=getpos(".")
		let l:pos_saved=deepcopy(l:pos)
		if g:atp_debugCloseLastEnvironment
		    let g:pos_saved0 = copy(l:pos_saved)
		endif

		while l:line_nr >= 0
			let l:line_nr=search('\%(%.*\)\@<!\\begin\s*{','bW')
		    " match last environment openned in this line.
		    " ToDo: afterwards we can make it works for multiple openned
		    " envs.
		    let l:env_name=matchstr(getline(l:line_nr),'\%(%.*\)\@<!\\begin\s*{\zs[^}]*\ze}\%(.*\\begin\s*{[^}]*}\)\@!')
		    if index(g:atp_long_environments,l:env_name) != -1
			let l:limit=3
		    else
			let l:limit=2
		    endif
		    let l:close_line_nr=atplib#CheckClosed('\%(%.*\)\@<!\\begin\s*{' . l:env_name, 
				\ '\%(%.*\)\@<!\\end\s*{' . l:env_name,
				\ l:line_nr,g:atp_completion_limits[l:limit],1)

		    if l:close_line_nr != 0
			call add(l:cenv_lines,l:close_line_nr)
		    else
			break
		    endif
		    let l:line_nr-=1
		endwhile

		keepjumps call setpos(".",l:pos)
			
		if getline(l:line_nr) =~ '\%(%.*\)\@<!\%(\\def\|\%(re\)\?newcommand\)' && l:line_nr != line(".")
" 		    let b:cle_return="def"
		    if g:atp_debugCloseLastEnvironment
			silent echo "return C"
			redir END
		    endif
		    return
		endif

		" get all names of environments which begin in this line
		let l:env_names=[]
		let l:line=getline(l:line_nr)
		while l:line =~ '\\begin\s*{' 
		    let l:cenv_begins = match(l:line,'\%(%.*\)\@<!\\begin{\zs[^}]*\ze}\%(.*\\begin\s{\)\@!')
		    let l:cenv_name = matchstr(l:line,'\%(%.*\)\@<!\\begin{\zs[^}]*\ze}\%(.*\\begin\s{\)\@!')
		    let l:cenv_len=len(l:cenv_name)
		    let l:line=strpart(l:line,l:cenv_begins+l:cenv_len)
		    call add(l:env_names,l:cenv_name)
			" DEBUG:
" 			let g:env_names=l:env_names
" 			let g:line=l:line
" 			let g:cenv_begins=l:cenv_begins
" 			let g:cenv_name=l:cenv_name
		endwhile
		" thus we have a list of env names.
		
		" make a dictionary of lines where they closes. 
		" this is a list of pairs (I need the order!)
		let l:env_dict=[]

		" list of closed environments
		let l:cenv_names=[]

		for l:uenv in l:env_names
		    let l:uline_nr=atplib#CheckClosed('\%(%.*\)\@<!\\begin\s*{' . l:uenv . '}', 
				\ '\%(%.*\)\@<!\\end\s*{' . l:uenv . '}', l:line_nr, g:atp_completion_limits[2])
		    call extend(l:env_dict,[ l:uenv, l:uline_nr])
		    if l:uline_nr != '0'
			call add(l:cenv_names,l:uenv)
		    endif
		endfor
		
		" close unclosed environment

		" check if at least one of them is closed
		if len(l:cenv_names) == 0
		    let l:str=""
		    for l:uenv in l:env_names
			if index(g:atp_no_complete,l:uenv) == '-1'
			    let l:str.='\end{' . l:uenv .'}'
			endif
		    endfor
		    " l:uenv will remain the last environment name which
		    " I use!
		    " Do not append empty lines (l:str is empty if all l:uenv
		    " belongs to the g:atp_no_complete list.
		    if len(l:str) == 0
			if g:atp_debugCloseLastEnvironment
			    silent echo "return D"
			    redir END
			endif
			return 0
		    endif
		    let l:eindent=atplib#CopyIndentation(getline(l:line_nr))
		    let l:pos=getpos(".")
		    if len(l:cenv_lines) > 0 

			let l:max=max(l:cenv_lines)
			let l:pos[1]=l:max+1
			" find the first closed item below the last closed
			" pair (below l:pos[1]). (I assume every env is in
			" a seprate line!
			let l:end=atplib#CheckClosed('\%(%.*\)\@<!\\begin\s*{','\%(%.*\)\@<!\\end\s*{',l:line_nr,g:atp_completion_limits[2],1)
			if g:atp_debugCloseLastEnvironment
			    let g:info= " l:max=".l:max." l:end=".l:end." line('.')=".line(".")." l:line_nr=".l:line_nr
			endif
			" if the line was found append just befor it.
			if l:end != 0 
				if line(".") <= l:max
				    if line(".") <= l:end
					call append(l:max, l:eindent . l:str)
					echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . l:end+1  
					call setpos(".",[0,l:max+1,len(l:eindent.l:str)+1,0])
				    else
					call append(l:end-1, l:eindent . l:str)
					echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . l:end+1 
					call setpos(".",[0,l:end,len(l:eindent.l:str)+1,0])
				    endif
				elseif line(".") < l:end
				    let [ lineNr, pos_lineNr ]	= getline(".") =~ '^\s*$' ? [ line(".")-1, line(".")] : [ line("."), line(".")+1 ]
				    call append(lineNr, l:eindent . l:str)
				    echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . line(".")+1  
				    call setpos(".",[0, pos_lineNr,len(l:eindent.l:str)+1,0])
				elseif line(".") >= l:end
				    call append(l:end-1, l:eindent . l:str)
				    echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . l:end
				    call setpos(".",[0,l:end,len(l:eindent.l:str)+1,0])
				endif
			else
			    if line(".") >= l:max
				call append(l:pos_saved[1], l:eindent . l:str)
				keepjumps call setpos(".",l:pos_saved)
				echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . line(".")+1
				call setpos(".",[0,l:pos_saved[1]+1,len(l:eindent.l:str)+1,0])
			    elseif line(".") < l:max
				call append(l:max, l:eindent . l:str)
				echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . l:max+1
				call setpos(".",[0,l:max+1,len(l:eindent.l:str)+1,0])
			    endif
			endif
		    else
			let l:pos[1]=l:line_nr
			let l:pos[2]=1
			" put cursor at the end of the line where not closed \begin was
			" found
			keepjumps call setpos(".",[0,l:line_nr,len(getline(l:line_nr)),0])
			let l:cline	= getline(l:pos_saved[1])
			if g:atp_debugCloseLastEnvironment
			    let g:cline		= l:cline
			    let g:pos_saved 	= copy(l:pos_saved)
			    let g:line		= l:pos_saved[1]
			endif
			let l:iline=searchpair('\\begin{','','\\end{','nW')
			if l:iline > l:line_nr && l:iline <= l:pos_saved[1]
			    call append(l:iline-1, l:eindent . l:str)
			    echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . l:iline
			    let l:pos_saved[2]+=len(l:str)
			    call setpos(".",[0,l:iline,len(l:eindent.l:str)+1,0])
			else
			    if l:cline =~ '\\begin{\%('.l:uenv.'\)\@!'
				call append(l:pos_saved[1]-1, l:eindent . l:str)
				echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . l:pos_saved[1]
				let l:pos_saved[2]+=len(l:str)
				call setpos(".",[0,l:pos_saved[1],len(l:eindent.l:str)+1,0])
			    elseif l:cline =~ '^\s*$'
				call append(l:pos_saved[1]-1, l:eindent . l:str)
				echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . l:pos_saved[1]
				let l:pos_saved[2]+=len(l:str)
				call setpos(".",[0,l:pos_saved[1]+1,len(l:eindent.l:str)+1,0])
			    else
				call append(l:pos_saved[1], l:eindent . l:str)
				echomsg "[ATP:] closing " . l:env_name . " from line " . l:bpos_env[0] . " at line " . l:pos_saved[1]+1
				" Do not move corsor if: '\begin{env_name}<Tab>'
				if l:cline !~  '\\begin\s*{\s*\%('.l:uenv.'\)\s*}'
				    let l:pos_saved[2]+=len(l:str)
				    call setpos(".",[0,l:pos_saved[1]+1,len(l:eindent.l:str)+1,0])
				else
				    call setpos(".", l:pos_saved)
				endif
			    endif
			endif 
			if g:atp_debugCloseLastEnvironment
			    silent echo "return E"
			    redir END
			endif
			return 1
		    endif
		else
		    if g:atp_debugCloseLastEnvironment
			silent echo "return F"
			redir END
		    endif
		    return "this is too hard?"
		endif
		unlet! l:env_names
		unlet! l:env_dict
		unlet! l:cenv_names
		unlet! l:pos 
		unlet! l:pos_saved
" 		if getline('.') =~ '^\s*$'
" 		    exec "normal dd"
		endif
    "}}}3
    "{{{2 close math: texMathZoneV, texMathZoneW, texMathZoneX, texMathZoneY 
    else
	"{{{3 Close math in the current line
	echomsg "[ATP:] closing math from line " . l:begin_line
	if    math_mode == 'texMathZoneV' && l:line !~ '^\s*\\(\s*$' 	||
	    \ math_mode == 'texMathZoneW' && l:line !~ '^\s*\\\[\s*$' 	||
	    \ math_mode == 'texMathZoneX' && l:line !~ '^\s*\$\s*$' 	||
	    \ math_mode == 'texMathZoneY' && l:line !~ '^\s*\$\{2,2}\s*$'
	    if math_mode == "texMathZoneW"
	 	if l:com == 'a' 
		    if getline(l:begin_line) =~ '^\s*\\\[\s*$'
			call append(line("."),atplib#CopyIndentation(getline(l:begin_line)).'\]')
		    else
			call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\]'. strpart(l:cline,getpos(".")[2]))
		    endif
		else
		    if getline(l:begin_line) =~ '^\s*\\\[\s*$'
			call append(line("."),atplib#CopyIndentation(getline(l:begin_line)).'\]')
		    else
			call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\]'. strpart(l:cline,getpos(".")[2]-1))
" TODO: This could be optional: (but the option rather
" should be an argument of this function rather than
" here!
		    endif
		    let l:pos=getpos(".")
		    let l:pos[2]+=2
		    keepjumps call setpos(("."),l:pos)
		    let b:cle_return="texMathZoneW"
		endif
	    elseif math_mode == "texMathZoneV"
		if l:com == 'a'
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]) . '\)'. strpart(l:cline,getpos(".")[2]))
		else
		    call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '\)'. strpart(l:cline,getpos(".")[2]-1))
		    call cursor(line("."),col(".")+2)
		    let b:cle_return="V"
		endif
	    elseif math_mode == "texMathZoneX" 
		call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '$'. strpart(l:cline,getpos(".")[2]-1))
		call cursor(line("."),col(".")+1)
	    elseif math_mode == "texMathZoneY" 
		call setline(line("."), strpart(l:cline,0,getpos(".")[2]-1) . '$$'. strpart(l:cline,getpos(".")[2]-1))
		call cursor(line("."),col(".")+2)
	    endif " }}}3
	"{{{3 Close math in a new line, preserv the indentation.
	else 	    
	    let l:eindent=atplib#CopyIndentation(l:line)
	    if math_mode == 'texMathZoneW'
		let l:iline=line(".")
		" if the current line is empty append before it.
		if getline(".") =~ '^\s*$' && l:iline > 1
		    let l:iline-=1
		endif
		call append(l:iline, l:eindent . '\]')
		echomsg "[ATP:] \[ closed in line " . l:iline
" 		let b:cle_return=2 . " dispalyed math " . l:iline  . " indent " . len(l:eindent) " DEBUG
	    elseif math_mode == 'texMathZoneV'
		let l:iline=line(".")
		" if the current line is empty append before it.
		if getline(".") =~ '^\s*$' && l:iline > 1
		    let l:iline-=1
		endif
		call append(l:iline, l:eindent . '\)')
		echomsg "[ATP:] \( closed in line " . l:iline
" 		let b:cle_return=2 . " inline math " . l:iline . " indent " .len(l:eindent) " DEBUG
	    elseif math_mode == 'texMathZoneX'
		let l:iline=line(".")
		" if the current line is empty append before it.
		if getline(".") =~ '^\s*$' && l:iline > 1
		    let l:iline-=1
		endif
		let sindent=atplib#CopyIndentation(getline(search('\$', 'bnW')))
		call append(l:iline, sindent . '$')
		echomsg "[ATP:] $ closed in line " . l:iline
	    elseif math_mode == 'texMathZoneY'
		let l:iline=line(".")
		" if the current line is empty append before it.
		if getline(".") =~ '^\s*$' && l:iline > 1
		    let l:iline-=1
		endif
		let sindent=atplib#CopyIndentation(getline(search('\$\$', 'bnW')))
		call append(l:iline, sindent . '$$')
		echomsg "[ATP:] $ closed in line " . l:iline
	    endif
	endif "}}3
    endif
    if g:atp_debugCloseLastEnvironment
	silent echo "return G"
	redir END
    endif
    "}}}2
endfunction
" imap <F7> <Esc>:call atplib#CloseLastEnvironment()<CR>
" }}}1
" Close Last Bracket
" {{{1 atplib#CloseLastBracket
"
" The first function only tests if there is a bracket to be closed.
" {{{2 				atplib#CheckBracket
" Returns a list [ l:open_line, l:open_col, l:open_bracket ] 
" l:open_col != 0 if any of brackets (in values(g:atp_bracket_dict) ) is
" opened and not closed. l:open_bracket is the most recent such bracket
" ([ l:open_line, l:open_col ] are its coordinates). 
"
" a:bracket_dict is a dictionary of brackets to use: 
" 	 	{ open_bracket : close_bracket } 
function! atplib#CheckBracket(bracket_dict)
    
    let limit_line	= max([1,(line(".")-g:atp_completion_limits[1])])
    let pos_saved 	= getpos(".")

    " Bracket sizes:
    let ket_pattern	= '\%(' . join(values(filter(copy(g:atp_sizes_of_brackets), "v:val != '\\'")), '\|') . '\)'


   " But maybe we shouldn't check if the bracket is closed sometimes one can
   " want to close closed bracket and delete the old one.
   
   let check_list = []
   if g:atp_debugCheckBracket
       call atplib#Log("CheckBracket.log","", "init")
       let g:check_list	= check_list
   endif

    "    change the position! and then: 
    "    check the flag 'r' in searchpair!!!
    let i=0
    let bracket_list= keys(a:bracket_dict)
    for ket in bracket_list
	let pos		= deepcopy(pos_saved)
	let pair_{i}	= searchpairpos(escape(ket,'\[]'),'', escape(a:bracket_dict[ket], '\[]'). 
		    \ ( ket_pattern != "" ? '\|'.ket_pattern.'\.' : '' ) ,'bnW',"",limit_line)
	if g:atp_debugCheckBracket >= 2
	    echomsg escape(ket,'\[]') . " pair_".i."=".string(pair_{i}) . " limit_line=" . limit_line
	endif
	let pos[1]	= pair_{i}[0]
	let pos[2]	= pair_{i}[1]
	" check_{i} is 1 if the bracket is closed
	let check_{i}	= atplib#CheckClosed(escape(ket, '\[]'), escape(a:bracket_dict[ket], '\[]'), line("."), g:atp_completion_limits[0],1) == '0'
	" check_dot_{i} is 1 if the bracket is closed with a dot (\right.) . 
	let check_dot_{i} = atplib#CheckClosed(escape(ket, '\'), '\\\%(right\|\cb\Cig\{1,2}\%(g\|l\)\@!r\=\)\s*\.',line("."),g:atp_completion_limits[0],1) == '0'
	if g:atp_debugCheckBracket >= 2
	    echomsg escape(ket,'\[]') . " check_".i."=".string(check_{i}) . " check_dot_".i."=".string(check_dot_{i})
	endif
	let check_{i}	= min([check_{i}, check_dot_{i}])
	call add(check_list, [ pair_{i}[0], (check_{i}*pair_{i}[1]), i ] ) 
	keepjumps call setpos(".",pos_saved)
	let i+=1
    endfor
    keepjumps call setpos(".", pos_saved)
   
    " Find opening line and column numbers
    call sort(check_list, "atplib#CompareCoordinates")
    let g:check_list = check_list
    let [ open_line, open_col, open_bracket_nr ] 	= check_list[0]
    let [ s:open_line, s:open_col, s:opening_bracket ] 	= [ open_line, open_col, bracket_list[open_bracket_nr] ]
    if g:atp_debugCheckBracket
	let [ g:open_lineCB, g:open_colCB, g:opening_bracketCB ] = [ open_line, open_col, bracket_list[open_bracket_nr] ]
	call atplib#Log("CheckBracket.log", "return:")
	call atplib#Log("CheckBracket.log", "open_line=".open_line)
	call atplib#Log("CheckBracket.log", "open_col=".open_col)
	call atplib#Log("CheckBracket.log", "opening_bracketCB=".g:opening_bracketCB)
    endif
    return [ open_line, open_col, bracket_list[open_bracket_nr] ]
endfunction
" }}}2
" The second function closes the bracket if it was not closed. 
" (as returned by atplib#CheckBracket or [ s:open_line, s:open_col, s:opening_bracket ])

" It is not used to close \(:\) and \[:\] as CloseLastEnvironment has a better
" way of doing that (preserving indentation)
" {{{2 			atplib#CloseLastBracket	
" a:bracket_dict is a dictionary of brackets to use: 
" 	 	{ open_bracket : close_bracket } 
" a:1 = 1 just return the bracket 
" a:2 = 0 (default), 1 when used in TabCompletion 
" 			then s:open_line, s:open_col and s:opening_bracket are
" 			used to avoid running twice atplib#CheckBracket():
" 			once in TabCompletion and secondly in CloseLastBracket
" 			function.

function! atplib#CloseLastBracket(bracket_dict, ...)
    
    let only_return	= ( a:0 >= 1 ? a:1 : 0 )
    let tab_completion	= ( a:0 >= 2 ? a:2 : 0 )

    " {{{3 preambule
    let pattern		= ""
    let size_patterns	= []
    for size in keys(g:atp_sizes_of_brackets)
	call add(size_patterns,escape(size,'\'))
    endfor

    let pattern_b	= '\C\%('.join(size_patterns,'\|').'\)'
    let pattern_o	= '\%('.join(map(keys(a:bracket_dict),'escape(v:val,"\\[]")'),'\|').'\)'

    if g:atp_debugCloseLastBracket
	call atplib#Log("CloseLastBracket.log","","init")
	let g:pattern_b	= pattern_b
	let g:pattern_o	= pattern_o
	call atplib#Log("CloseLastBracket.log", "pattern_b=".pattern_b)
	call atplib#Log("CloseLastBracket.log", "pattern_o=".pattern_o)
    endif

    let limit_line	= max([1,(line(".")-g:atp_completion_limits[1])])
        
    let pos_saved 	= getpos(".")


   " But maybe we shouldn't check if the bracket is closed sometimes one can
   " want to close closed bracket and delete the old one.
   
    let [ open_line, open_col, opening_bracket ] = ( tab_completion ? 
		\ deepcopy([ s:open_line, s:open_col, s:opening_bracket ]) : atplib#CheckBracket(a:bracket_dict) )

    " Check and Close Environment:
	for env_name in g:atp_closebracket_checkenv
     	    " To Do: this should check for the most recent opened environment
	    let limit_line 	= exists("open_line") ? open_line : search('\\\@<!\\\[\|\\\@<!\\(\|\$', 'bn')
	    let open_env 	= searchpairpos('\\begin\s*{\s*'.env_name.'\s*}', '', '\\end\s*{\s*'.env_name.'\s*}', 'bnW', '', limit_line)
	    let env_name 	= matchstr(strpart(getline(open_env[0]),open_env[1]-1), '\\begin\s*{\s*\zs[^}]*\ze*\s*}')
	    if open_env[0] && atplib#CompareCoordinates([(exists("open_line") ? open_line : 0),(exists("open_line") ? open_col : 0)], open_env)
		call atplib#CloseLastEnvironment('i', 'environment', env_name, open_env)
		return 'closeing ' . env_name . ' at ' . string(open_env) 
	    endif
	endfor

   " Debug:
   if g:atp_debugCloseLastBracket
       let g:open_line	= open_line
       let g:open_col	= open_col 
       call atplib#Log("CloseLastBracket.log", "open_line=".open_line)
       call atplib#Log("CloseLastBracket.log", "open_col=".open_col)
   endif

    "}}}3
    " {{{3 main if statements

   if getline(open_line)[open_col-3] . getline(open_line)[open_col-2] . getline(open_line)[open_col-1] =~ '\\\@<!\\\%((\|\[\)$'
       call atplib#CloseLastEnvironment('i', 'math', '', [ open_line, open_col ])
       if g:atp_debugCloseLastBracket
	   let b:atp_debugCLB = "call atplib#CloseLastEnvironment('i', 'math', '', [ ".open_line.", ".open_col." ])"
	   call atplib#Log("CloseLastBracket.log", "calling atplib#CloseLastEnvironment('i', 'math', '', [ ".open_line.", ".open_col." ])")
       endif
       return
   endif

   if open_col 
	let line	= getline(open_line)
	let bline	= strpart(line,0,(open_col-1))
	let eline	= strpart(line,open_col-1,2)
	if g:atp_debugCloseLastBracket
	    let g:bline = bline
	    let g:eline = eline
	    call atplib#Log("CloseLastBracket.log", "bline=".bline)
	    call atplib#Log("CloseLastBracket.log", "eline=".eline)
	endif

	let opening_size=matchstr(bline,'\zs'.pattern_b.'\ze\s*$')
	let closing_size=get(g:atp_sizes_of_brackets,opening_size,"")
" 	let opening_bracket=matchstr(eline,'^'.pattern_o)
" 	let opening_bracket=bracket_list[open_bracket_nr]

	if opening_size =~ '\\' && opening_bracket != '(' && opening_bracket != '['
	    let bbline		= strpart(bline, 0, len(bline)-1)
	    let opening_size2	= matchstr(bbline,'\zs'.pattern_b.'\ze\s*$')
	    let closing_size2	= get(g:atp_sizes_of_brackets,opening_size2,"")
	    let closing_size	= closing_size2.closing_size

	    " DEBUG
	    if g:atp_debugCloseLastBracket
		let g:bbline		= bbline
		let g:opening_size2	= opening_size2
		let g:closing_size2	= closing_size2
		call atplib#Log("CloseLastBracket.log", "bbline=".bbline)
		call atplib#Log("CloseLastBracket.log", "opening_size2=".opening_size2)
		call atplib#Log("CloseLastBracket.log", "closing_size2=".closing_size2)
	    endif
	endif

	echomsg "[ATP:] closing " . opening_size . opening_bracket . " from line " . open_line

	" DEBUG:
	if g:atp_debugCloseLastBracket
	    let g:o_bra		= opening_bracket
	    call atplib#Log("CloseLastBracket.log", "opening_bracket=".opening_bracket)
	    let g:o_size	= opening_size
	    call atplib#Log("CloseLastBracket.log", "opening_size=".opening_size)
	    let g:bline		= bline
	    call atplib#Log("CloseLastBracket.log", "bline=".bline)
	    let g:line		= line
	    call atplib#Log("CloseLastBracket.log", "line=".line)
	    let g:eline		= eline
	    call atplib#Log("CloseLastBracket.log", "eline=".eline)
	    let g:opening_size	= opening_size
	    call atplib#Log("CloseLastBracket.log", "opening_size=".opening_size)
	    let g:closing_size	= closing_size
	    call atplib#Log("CloseLastBracket.log", "closing_size=".closing_size)
	endif

	let cline=getline(line("."))
	if mode() == 'i'
	    if !only_return
		call setline(line("."), strpart(cline, 0, getpos(".")[2]-1).
			\ closing_size.get(a:bracket_dict, opening_bracket). 
			\ strpart(cline,getpos(".")[2]-1))
	    endif
	    let l:return=closing_size.get(a:bracket_dict, opening_bracket)
	elseif mode() == 'n'
	    if !only_return
		call setline(line("."), strpart(cline,0,getpos(".")[2]).
			\ closing_size.get(a:bracket_dict,opening_bracket). 
			\ strpart(cline,getpos(".")[2]))
	    endif
	    let l:return=closing_size.get(a:bracket_dict, opening_bracket)
	endif
	let pos=getpos(".")
	let pos[2]+=len(closing_size.get(a:bracket_dict, opening_bracket))
	keepjumps call setpos(".", pos)

	return l:return
   endif
   " }}}3
endfunction
" }}}2
" }}}1

" Tab Completion:
" atplib#TabCompletion {{{1
" This is the main TAB COMPLITION function.
"
" expert_mode = 1 (on)  gives less completions in some cases (commands,...)
" 			the matching pattern has to match at the beginning and
" 			is case sensitive. Furthermode  in expert mode, if
" 			completing a command and found less than 1 match then
" 			the function tries to close \(:\) or \[:\] (but not an
" 			environment, before doing ToDo in line 3832 there is
" 			no sense to make it).
" 			<Tab> or <F7> (if g:atp_no_tab_map=1)
" expert_mode = 0 (off) gives more matches but in some cases better ones, the
" 			string has to match somewhare and is case in
" 			sensitive, for example:
" 			\arrow<Tab> will show all the arrows definded in tex,
" 			in expert mode there would be no match (as there is no
" 			command in tex which begins with \arrow).
" 			<S-Tab> or <S-F7> (if g:atp_no_tab_map=1)
"
" Completion Modes:
" 	documentclass (\documentclass)
" 	labels   (\ref,\eqref)
" 	packages (\usepackage)
" 	commands
" 	environments (\begin,\(:\),\[:\])
" 	brackets ((:),[:],{:}) preserves the size operators!
" 		Always: check first brackets then environments. Bracket
" 		funnction can call function which closes environemnts but not
" 		vice versa.
" 	bibitems (\cite\|\citep\|citet)
" 	bibfiles (\bibliography)
" 	bibstyle (\bibliographystyle)
" 	end	 (close \begin{env} with \end{env})
" 	font encoding
" 	font family
" 	font series
" 	font shape
" 
"ToDo: the completion should be only done if the completed text is different
"from what it is. But it might be as it is, there are reasons to keep this.
"
try
" Main tab completion function
function! atplib#TabCompletion(expert_mode,...)

    if g:atp_debugTabCompletion
	call atplib#Log("TabCompletion.log", "", "init")
    endif

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    " {{{2 Match the completed word 
    let normal_mode=0

    if a:0 >= 1
	let normal_mode=a:1
    endif

    " this specifies the default argument for atplib#CloseLastEnvironment()
    " in some cases it is better to append after than before.
    let append='i'

    " Define string parts used in various completitons
    let pos		= getpos(".")
    let pos_saved	= deepcopy(pos)
    let line		= join(getbufline("%",pos[1]))
    let nchar		= strpart(line,pos[2]-1,1)
"     let rest		= strpart(line,pos[2]-1) 
    let l		= strpart(line,0,pos[2]-1)
    let n		= strridx(l,'{')
    let m		= strridx(l,',')
    let o		= strridx(l,'\')
    let s		= strridx(l,' ')
    let p		= strridx(l,'[')
    let r		= strridx(l,'=')
    let c		= match(l, '\\cite\>\(.*\\cite\>\)\@!') 
    let a		= len(l) - stridx(join(reverse(split(l, '\zs')), ''), "=")
     
    let nr=max([n,m,o,s,p])
    let color_nr=max([nr, r])

    " this matches for =...
    let abegin		= strpart(l, a-1)

    " this matches for \...
    let begin		= strpart(l,nr+1)
    let color_begin	= strpart(l,color_nr+1)
    let cbegin		= strpart(l,nr)
    " and this for '\<\w*$' (beginning of last started word) -- used in
    " tikzpicture completion method 
    let tbegin		= matchstr(l,'\zs\<\w*$')
    " start with last '\'
    let obegin		= strpart(l,o)

    " what we are trying to complete: usepackage, environment.
    let pline		= strpart(l, 0, nr)
    	" \cite[Theorem~1]{Abcd -> \cite[Theorem~] 
    let ppline		= strpart(l, c)
    	" \cite[Theorem~1]{Abcd -> \cite[Theorem~1]{ 

    let limit_line=max([1,(pos[1]-g:atp_completion_limits[1])])

    if g:atp_debugTabCompletion
	let g:nchar	= nchar
	call atplib#Log("TabCompletion.log", "nchar=".nchar)
	let g:l		= l
	call atplib#Log("TabCompletion.log", "l=".l)
	let g:n		= n
	call atplib#Log("TabCompletion.log", "n=".n)
	let g:o		= o
	call atplib#Log("TabCompletion.log", "o=".o)
	let g:s		= s
	call atplib#Log("TabCompletion.log", "s=".s)
	let g:p		= p
	call atplib#Log("TabCompletion.log", "p=".p)
	let g:a		= a
	call atplib#Log("TabCompletion.log", "a=".a)
	let g:nr	= nr
	call atplib#Log("TabCompletion.log", "nr=".nr)

	let g:line	= line    
	call atplib#Log("TabCompletion.log", "line=".line)
	let g:abegin	= abegin
	call atplib#Log("TabCompletion.log", "abegin=".abegin)
	let g:tbegin	= tbegin
	call atplib#Log("TabCompletion.log", "tbegin=".tbegin)
	let g:cbegin	= cbegin
	call atplib#Log("TabCompletion.log", "cbegin=".cbegin)
	let g:obegin	= obegin
	call atplib#Log("TabCompletion.log", "obegin=".obegin)
	let g:begin	= begin 
	call atplib#Log("TabCompletion.log", "begin=".begin)
	let g:pline	= pline
	call atplib#Log("TabCompletion.log", "pline=".pline)
	let g:ppline	= ppline
	call atplib#Log("TabCompletion.log", "ppline=".ppline)

	let g:limit_line= limit_line
	call atplib#Log("TabCompletion.log", "limit_line=".limit_line)
    endif


" {{{2 SET COMPLETION METHOD
    " {{{3 --------- command
    if o > n && o > s && 
	\ pline !~ '\%(input\s*{[^}]*$\|include\%(only\)\=\s*{[^}]*$\|[^\\]\\\\[^\\]$\)' &&
	\ pline !~ '\\\@<!\\$' &&
	\ begin !~ '{\|}\|,\|-\|\^\|\$\|(\|)\|&\|-\|+\|=\|#\|:\|;\|\.\|,\||\|?$' &&
	\ begin !~ '^\[\|\]\|-\|{\|}\|(\|)' &&
	\ cbegin =~ '^\\' && !normal_mode &&
	\ l !~ '\\\%(no\)\?cite[^}]*$' &&
	\ l !~ '\\ref\s*{\S*$'

	" in this case we are completing a command
	" the last match are the things which for sure do not ends any
	" command. The pattern '[^\\]\\\\[^\\]$' do not matches "\" and "\\\",
	" in which case the line contains "\\" and "\\\\" ( = line ends!)
	" (here "\" is one character \ not like in magic patterns '\\')
	" but matches "" and "\\" (i.e. when completing "\" or "\\\" [end line
	" + command].
	if index(g:atp_completion_active_modes, 'commands') != -1
	    let completion_method='command'
	    " DEBUG:
	    let b:comp_method='command'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
" 	    let b:comp_method='command fast return'
	    return ''
	endif
    "{{{3 --------- environment names
    elseif (pline =~ '\%(\\begin\|\\end\)\s*$' && begin !~ '}.*$' && !normal_mode)
	if index(g:atp_completion_active_modes, 'environment names') != -1 
	    let completion_method='environment_names'
	    " DEBUG:
	    let b:comp_method='environment_names'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
" 	    let b:comp_method='environment_names fast return'
	    return ''
	endif
    "{{{3 --------- colors
    elseif l =~ '\\\%(textcolor\|pagecolor\){[^}]*$\|\<\%(backgroundcolor\|bordercolor\|color\|linecolor\)=\s*\w*$'
	" this supports todonotes \todo command.
	let completion_method='colors'
	" DEBUG:
	let b:comp_method='colors'
	call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
    "{{{3 --------- label
    elseif l =~ '\\\%(eq\)\?ref\s*{[^}]*$' && !normal_mode
	if index(g:atp_completion_active_modes, 'labels') != -1 
	    let completion_method='labels'
	    " DEBUG:
	    let b:comp_method='labels'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='labels fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- pagestyle
    elseif l =~ '\\\%(pagestyle\|thispagestyle\){\s*$'
	let completion_method='pagestyle'
	" DEBUG:
	let b:comp_method='pagestyle'
	call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
    "{{{3 --------- pagenumbering
    elseif l =~ '\\pagenumbering{\s*$'
	let completion_method='pagenumbering'
	" DEBUG:
	let b:comp_method='pagenumbering'
	call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
    "{{{3 --------- bibitems
    elseif ppline =~ '\\\%(no\)\?cite\(\s*\[[^]]*\]\s*\)\={[^}]*$' && !normal_mode
	if index(g:atp_completion_active_modes, 'bibitems') != -1
	    let completion_method='bibitems'
	    " DEBUG:
	    let b:comp_method='bibitems'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='bibitems fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- tikzpicture
    elseif ( search('\%(\\def\>.*\|\\\%(re\)\?newcommand\>.*\|%.*\)\@<!\\begin{tikzpicture}','bnW') > search('[^%]*\\end{tikzpicture}','bnW') ||
	\ !atplib#CompareCoordinates(searchpos('[^%]*\zs\\tikz{','bnw'),searchpos('}','bnw')) )
" 	\ && ( l =~ '\%(\s\|\[\|{\|}\|,\|\.\|=\|:\)' . tbegin . '$' || l =~ '\\' . tbegin  . '$' )
	"{{{4 ----------- tikzpicture keywords
	if l =~ '\%(\s\|\[\|{\|}\|,\|\.\|=\|:\)' . tbegin . '$' && !normal_mode
	    if index(g:atp_completion_active_modes, 'tikzpicture keywords') != -1 
		" DEBUG:
		let b:comp_method='tikzpicture keywords'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		let completion_method="tikzpicture keywords"
	    else
		let b:comp_method='tikzpicture keywords fast return'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		return ''
	    endif
	"{{{4 ----------- tikzpicture commands
	elseif  l =~ '\\' . tbegin  . '$' && !normal_mode
	    if index(g:atp_completion_active_modes, 'tikzpicture commands') != -1
		" DEBUG:
		let b:comp_method='tikzpicture commands'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		let completion_method="tikzpicture commands"
	    else
		let b:comp_method='tikzpicture commands fast return'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		return ''
	    endif
	"{{{4 ----------- close_env tikzpicture
	else
	    let begParen = atplib#CheckBracket(g:atp_bracket_dict)
	    if begParen[0]
		return g:atp_bracket_dict[begParen[2]]
	    endif
	    if (!normal_mode &&  index(g:atp_completion_active_modes, 'close environments') != -1 ) ||
			\ (normal_mode && index(g:atp_completion_active_modes_normal_mode, 'close environments') != -1 )
		" DEBUG:
		let b:comp_method='close_env tikzpicture'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		let completion_method="close_env"
	    else
		let b:comp_method='close_env tikzpicture fast return'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		return ''
	    endif
	endif
    "{{{3 --------- package
    elseif pline =~ '\\usepackage\%([.*]\)\?\s*' && !normal_mode
	if index(g:atp_completion_active_modes, 'package names') != -1
	    let completion_method='package'
	    " DEBUG:
	    let b:comp_method='package'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='package fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- tikz libraries
    elseif pline =~ '\\usetikzlibrary\%([.*]\)\?\s*' && !normal_mode
	if index(g:atp_completion_active_modes, 'tikz libraries') != -1
	    let completion_method='tikz libraries'
	    " DEBUG:
	    let b:comp_method='tikz libraries'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='tikz libraries fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- inputfiles
    elseif (l =~ '\\input\%([^{}]*\|\s*{[^}]*\)$'||
	  \ l =~ '\\include\s*{[^}]*$' ||
	  \ l =~ '\\includeonly\s*{[^}]*$') && !normal_mode 
	if begin =~ 'input'
	    let begin=substitute(begin,'.*\%(input\|include\%(only\)\?\)\s\?','','')
	endif
	if index(g:atp_completion_active_modes, 'input files') != -1
	    let completion_method='inputfiles'
	    " DEBUG:
	    let b:comp_method='inputfiles'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='inputfiles fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- bibfiles
    elseif pline =~ '\\\%(bibliography\%(style\)\@!\|addbibresource\|addglobalbib\)' && !normal_mode
	if index(g:atp_completion_active_modes, 'bibfiles') != -1
	    let completion_method='bibfiles'
	    " DEBUG:
	    let b:comp_method='bibfiles'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='bibfiles fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- bibstyles
    elseif pline =~ '\\bibliographystyle' && !normal_mode 
	if ( index(g:atp_completion_active_modes, 'bibstyles') != -1 ) 
	    let completion_method='bibstyles'
	    let b:comp_method='bibstyles'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='bibstyles fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- todo & missingfigure options
    elseif obegin =~ '\\todo\[[^\]]*'
	if ( index(g:atp_completion_active_modes, 'todonotes') != -1 ) 
	    let completion_method='todo options'
	    let b:comp_method='todo options'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='todo options fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    elseif obegin =~ '\\missingfigure\[[^\]]*'
	if ( index(g:atp_completion_active_modes, 'todonotes') != -1 ) 
	    let completion_method='missingfigure options'
	    let b:comp_method='missingfigure options'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='missingfigure options fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- documentclass
    elseif pline =~ '\\documentclass\>' && !normal_mode 
	if index(g:atp_completion_active_modes, 'documentclass') != -1
	    let completion_method='documentclass'
	    let b:comp_method='documentclass'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='documentclass fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- Beamer Themes
    elseif pline =~ '\\usetheme$' && !normal_mode
	if index(g:atp_completion_active_modes, 'beamerthemes') != -1
	    let completion_method='beamerthemes'
	    let b:comp_method='beamerthemes'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='beamerthemes fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- Beamer Inner Themes
    elseif pline =~ '\\useinnertheme$' && !normal_mode
	if index(g:atp_completion_active_modes, 'beamerinnerthemes') != -1
	    let completion_method='beamerinnerthemes'
	    let b:comp_method='beamerinnerthemes'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='beamerinnerthemes fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- Beamer Outer Themes
    elseif pline =~ '\\useoutertheme$' && !normal_mode
	if index(g:atp_completion_active_modes, 'beamerouterthemes') != -1
	    let completion_method='beamerouterthemes'
	    let b:comp_method='beamerouterthemes'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='beamerouterthemes fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- Beamer Color Themes
    elseif pline =~ '\\usecolortheme$' && !normal_mode
	if index(g:atp_completion_active_modes, 'beamercolorthemes') != -1
	    let completion_method='beamercolorthemes'
	    let b:comp_method='beamercolorthemes'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='beamercolorthemes fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- Beamer Font Themes
    elseif pline =~ '\\usefonttheme$' && !normal_mode
	if index(g:atp_completion_active_modes, 'beamerfontthemes') != -1
	    let completion_method='beamerfontthemes'
	    let b:comp_method='beamerfontthemes'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='beamerfontthemes fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- font family
    elseif l =~ '\%(\\usefont{[^}]*}{\|\\DeclareFixedFont{[^}]*}{[^}]*}{\|\\fontfamily{\)[^}]*$' && !normal_mode 
	if index(g:atp_completion_active_modes, 'font family') != -1
	    let completion_method='font family'
	    let b:comp_method='font family'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='font family fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- font series
    elseif l =~ '\%(\\usefont{[^}]*}{[^}]*}{\|\\DeclareFixedFont{[^}]*}{[^}]*}{[^}]*}{\|\\fontseries{\)[^}]*$' && !normal_mode 
	if index(g:atp_completion_active_modes, 'font series') != -1
	    let completion_method='font series'
	    let b:comp_method='font series'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='font series fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- font shape
    elseif l =~ '\%(\\usefont{[^}]*}{[^}]*}{[^}]*}{\|\\DeclareFixedFont{[^}]*}{[^}]*}{[^}]*}{[^}]*}{\|\\fontshape{\)[^}]*$' && !normal_mode 
	if index(g:atp_completion_active_modes, 'font shape') != -1
	    let completion_method='font shape'
	    let b:comp_method='font shape'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='font shape fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- font encoding
    elseif l =~ '\%(\\usefont{\|\\DeclareFixedFont{[^}]*}{\|\\fontencoding{\)[^}]*$' && !normal_mode 
	if index(g:atp_completion_active_modes, 'font encoding') != -1
	    let completion_method='font encoding'
	    let b:comp_method='font encoding'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	else
	    let b:comp_method='font encoding fast return'
	    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    return ''
	endif
    "{{{3 --------- brackets
    else
	let begParen = atplib#CheckBracket(g:atp_bracket_dict)
" 	let g:begParen = copy(begParen)
	if begParen[1] != 0
	    if (!normal_mode &&  index(g:atp_completion_active_modes, 'brackets') != -1 ) ||
		    \ (normal_mode && index(g:atp_completion_active_modes_normal_mode, 'brackets') != -1 )
		let b:comp_method='brackets'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		if atplib#CheckSyntaxGroups(['texMathZoneV'])
		    let pattern = '\\\@!\\('
		elseif atplib#CheckSyntaxGroups(['texMathZoneW'])
		    let pattern = '\\\@<!\\\['
		elseif atplib#CheckSyntaxGroups(['texMathZoneX'])
		    let pattern = '\%(\\\|\$\)\@<!\$\$\@!'
		elseif atplib#CheckSyntaxGroups(['texMathZoneY'])
		    let pattern = '\\\@<!\$\$'
		else
		    let pattern = ''
		endif
" 		let g:pattern = pattern
		if !empty(pattern)
		    let begMathZone = searchpos(pattern, 'bnW')
" 		    let g:begMathZone = copy(begMathZone)
		    if atplib#CompareCoordinates([ begParen[0], begParen[1] ], begMathZone)
			call atplib#CloseLastEnvironment(append, 'math')
		    else
			call atplib#CloseLastBracket(g:atp_bracket_dict, 0, 1)
		    endif
		else
		    call atplib#CloseLastBracket(g:atp_bracket_dict, 0, 1)
		endif
		return '' 
	    else
		let b:comp_method='brackets fast return'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		return ''
	    endif
    "{{{3 --------- algorithmic
	elseif atplib#CheckBracket(g:atp_algorithmic_dict)[1] != 0
		if atplib#CheckSyntaxGroups(['texMathZoneALG']) && (
			\ (!normal_mode && index(g:atp_completion_active_modes, 'algorithmic' ) != -1 ) ||
			\ (normal_mode && index(g:atp_completion_active_modes_normal_mode, 'algorithmic') != -1 )
			\ )
		    let b:comp_method='algorithmic'
		    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		    call atplib#CloseLastBracket(g:atp_algorithmic_dict, 0, 1)
		    return '' 
		else
		    let b:comp_method='algorithmic fast return'
		    call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		    return ''
		endif
    "{{{3 --------- abbreviations
    elseif l =~ '=\w\+\*\=$'
	let completion_method='abbreviations' 
	let b:comp_method='abbreviations'
	call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
    "{{{3 --------- close environments
	else
	    if (!normal_mode &&  index(g:atp_completion_active_modes, 'close environments') != '-1' ) ||
			\ (normal_mode && index(g:atp_completion_active_modes_normal_mode, 'close environments') != '-1' )
		let completion_method='close_env'
		" DEBUG:
		let b:comp_method='close_env a' 
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    else
		let b:comp_method='close_env a fast return' 
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
		return ''
	    endif
	endif
    endif
" if the \[ is not closed, first close it and then complete the commands, it
" is better as then automatic tex will have better file to operate on.
" {{{2 close environments
    if completion_method=='close_env'

	" Close one line math
	if atplib#CheckInlineMath('texMathZoneV') || 
		    \ atplib#CheckInlineMath('texMathZoneW') ||
		    \ atplib#CheckInlineMath('texMathZoneX') ||
		    \ b:atp_TexFlavor == 'plaintex' && atplib#CheckInlineMath('texMathZoneY')
	    let b:tc_return = "close_env math"
	    call atplib#CloseLastEnvironment(append, 'math')
	" Close environments
	else
	    let b:tc_return = "close_env environment"
	    let stopline_forward	= line(".") + g:atp_completion_limits[2]
	    let stopline_backward	= max([ 1, line(".") - g:atp_completion_limits[2]])

	    let line_nr=line(".")
	    while line_nr >= stopline_backward
		let line_nr 		= searchpair('\\begin\s*{', '', '\\end\s*{', 'bnW', 'strpart(getline("."), 0, col(".")-1) =~ "\\\\\\@<!%"', stopline_backward)
		if line_nr >= stopline_backward
		    let env_name	= matchstr(getline(line_nr), '\\begin\s*{\zs[^}]*}\ze}')
		    if env_name		=~# '^\s*document\s*$' 
			break
		    endif
		    let line_forward 	= searchpair('\\begin\s*{'.env_name.'}', '', '\\end\s*{'.env_name.'}', 
							\ 'nW', '', stopline_forward)
		    if line_forward == 0
			break
		    endif
			
		else
		    let line_nr = 0
		endif
	    endwhile

	    if line_nr
	    " the env_name variable might have wrong value as it is
	    " looking using '\\begin' and '\\end' this might be not enough, 
		" however the function atplib#CloseLastEnv works perfectly and this
		" should be save:

		if env_name !~# '^\s*document\s*$'
		    call atplib#CloseLastEnvironment(append, 'environment', '', [line_nr, 0])
		    return ""
		else
		    return ""
		endif
	    endif
	endif
	return ""
    endif
" {{{2 SET COMPLETION LIST
    " generate the completion names
    " {{{3 ------------ ENVIRONMENT NAMES
    if completion_method == 'environment_names'
	let end=strpart(line,pos[2]-1)

	keepjumps call setpos(".",[0,1,1,0])
	let stop_line=search('\\begin\s*{document}','cnW')
	keepjumps call setpos(".",pos_saved)

	if end !~ '\s*}'
	    let completion_list = []
	    if atplib#DocumentClass(b:atp_MainFile) == 'beamer'
		call extend(completion_list, g:atp_BeamerEnvironments)
	    endif
	    call extend(completion_list,deepcopy(g:atp_Environments))
	    if g:atp_local_completion
		" Make a list of local envs and commands
		if !exists("s:atp_LocalEnvironments") 
		    LocalCommands
		    let s:atp_LocalEnvironments=copy(b:atp_LocalEnvironments)
		elseif has("python")
		    LocalCommands
		    let s:atp_LocalEnvironments=copy(b:atp_LocalEnvironments)
		endif
		let completion_list=atplib#Extend(completion_list,s:atp_LocalEnvironments)
	    endif
	    let completion_list=atplib#Add(completion_list,'}')
	else
	    let completion_list = []
	    if atplib#DocumentClass(b:atp_MainFile) == 'beamer'
		call extend(completion_list, g:atp_BeamerEnvironments)
	    endif
	    call extend(completion_list,deepcopy(g:atp_Environments))
	    if g:atp_local_completion
		" Make a list of local envs and commands
		if !exists("s:atp_LocalEnvironments") 
		    LocalCommands
		    let s:atp_LocalEnvironments=copy(b:atp_LocalEnvironments)
		elseif has("python")
		    LocalCommands
		    let s:atp_LocalEnvironments=copy(b:atp_LocalEnvironments)
		endif
		call atplib#Extend(completion_list,s:atp_LocalEnvironments)
	    endif
	endif
	" TIKZ
	let in_tikz=searchpair('\\begin\s*{tikzpicture}','','\\end\s*{tikzpicture}','bnW',"", max([1,(line(".")-g:atp_completion_limits[2])])) || atplib#CheckOpened('\\tikz{','}',line("."),g:atp_completion_limits[0])
	if in_tikz
	    if end !~ '\s*}'
		call extend(completion_list,atplib#Add(g:atp_tikz_environments,'}'))
	    else
		call extend(completion_list,g:atp_tikz_environments)
	    endif
	endif
	" AMSMATH
	if atplib#SearchPackage('amsmath', stop_line) || g:atp_amsmath != 0 || atplib#DocumentClass(b:atp_MainFile) =~ '^ams'
	    if end !~ '\s*}'
		call extend(completion_list,atplib#Add(g:atp_amsmath_environments,'}'),0)
	    else
		call extend(completion_list,g:atp_amsmath_environments,0)
	    endif
	endif
	" MathTools
	if atplib#SearchPackage('mathtools', stop_line)
	    if end !~ '\s*}'
		call extend(completion_list,atplib#Add(g:atp_MathTools_environments,'}'))
	    else
		call extend(completion_list,g:atp_MathTools_environments)
	    endif
	endif
    "{{{3 ------------ PACKAGES
    elseif completion_method == 'package'
	if exists("g:atp_LatexPackages")
	    let completion_list	= copy(g:atp_LatexPackages)
	else
	    if g:atp_debugTabCompletion
		let debugTabCompletion_LatexPackages_TimeStart=reltime()
	    endif
	    let g:atp_LatexPackages	= atplib#KpsewhichGlobPath("tex", "", "*.sty")
	    let completion_list	= deepcopy(g:atp_LatexPackages)
	    if g:atp_debugTabCompletion
		let g:debugTabCompletion_LatexPackages_Time=reltimestr(reltime(debugTabCompletion_LatexPackages_TimeStart))
		call atplib#Log("TabCompletion.log", "LatexPackages Time: ".g:debugTabCompletion_LatexPackages_Time)
	    endif
	endif
    "{{{3 ------------ COLORS
    elseif completion_method == 'colors'
	" To Do: make a predefined lists of colors depending on package
	" options! 
	if !exists("b:atp_LocalColors") 
	    LocalCommands
	elseif has("python")
	    LocalCommands
	endif
	let completion_list=copy(b:atp_LocalColors)
    "{{{3 ------------ PAGESTYLE
    elseif completion_method == 'pagestyle'
	let completion_list=copy(g:atp_pagestyles)
	if atplib#SearchPackage('fancyhdr')
	    call extend(completion_list, g:atp_fancyhdr_pagestyles)
	endif
    "{{{3 ------------ PAGENUMBERING
    elseif completion_method == 'pagenumbering'
	let completion_list=copy(g:atp_pagenumbering)
    " {{{3 ------------ TIKZ LIBRARIES
    elseif completion_method == 'tikz libraries'
	let completion_list=deepcopy(g:atp_tikz_libraries)

    " {{{3 ------------ TIKZ KEYWORDS
    elseif completion_method == 'tikzpicture keywords'

	keepjumps call setpos(".",[0,1,1,0])
	let stop_line=search('\\begin\s*{document}','cnW')
	keepjumps call setpos(".",pos_saved)

	let completion_list=deepcopy(g:atp_tikz_keywords)
	" TODO: add support for all tikz libraries 
	let tikz_libraries	= atplib#GrepPackageList('\\use\%(tikz\|pgf\)library\s*{')
	call map(tikz_libraries, "substitute(v:val, '\\..*$', '', '')")
	let g:tikz_libraries  	= tikz_libraries
	let g:tikz_libs = []
	for lib in tikz_libraries  
	    if exists("g:atp_tikz_library_".lib."_keywords")
		call add(g:tikz_libs, lib)
		call extend(completion_list,g:atp_tikz_library_{lib}_keywords)
	    endif   
	endfor
    " {{{3 ------------ TIKZ COMMANDS
    elseif completion_method	== 'tikzpicture commands'
	let completion_list = []
	" if tikz is declared and we are in tikz environment.
	let tikz_libraries	= atplib#GrepPackageList('\\use\%(tikz\|pgf\)library\s*{')
	for lib in tikz_libraries  
	    if exists("g:atp_tikz_library_".lib."_commands")
		call extend(completion_list,g:atp_tikz_library_{lib}_keywords)
	    endif   
	endfor
    " {{{3 ------------ COMMANDS
    elseif completion_method == 'command'
	"{{{4 
	let tbegin=strpart(l,o+1)
	let completion_list=[]

	" Find end of the preambule.
	if expand("%:p") == atp_MainFile
	    " if the file is the main file
	    let saved_pos=getpos(".")
	    keepjumps call setpos(".", [0,1,1,0])
	    keepjumps let stop_line=search('\\begin\s*{document}','nW')
	    keepjumps call setpos(".", saved_pos)
	else
	    " if the file doesn't contain the preambule
	    if &filetype == 'tex'
		let saved_loclist	= getloclist(0)
		silent! execute '1lvimgrep /\\begin\s*{\s*document\s*}/j ' . fnameescape(atp_MainFile)
		let stop_line	= get(get(getloclist(0), 0, {}), 'lnum', 0)
		call setloclist(0, saved_loclist) 
	    else
		let stop_line = 0
	    endif
	endif
	 
	" Are we in the math mode?
	let math_is_opened	= atplib#CheckSyntaxGroups(g:atp_MathZones) && !atplib#CheckSyntaxGroups(['texMathText'])

   	"{{{4 -------------------- picture
	if searchpair('\\begin\s*{picture}','','\\end\s*{picture}','bnW',"", max([ 1, (line(".")-g:atp_completion_limits[2])]))
	    call extend(completion_list,g:atp_picture_commands)
	endif 
   	"{{{4 -------------------- hyperref
	if searchpair('\\\@<!{', '', '\\\@<!}', 'bnW', "", max([ 1, (line(".")-g:atp_completion_limits[0])]))
	    call extend(completion_list, g:atp_Commands)
	    if atplib#SearchPackage('herref')
		call extend(completion_list, g:atp_hyperref_commands)
	    endif
	endif
	" {{{4 -------------------- MATH commands: amsmath, amssymb, mathtools, nicefrac, SIunits, math non expert mode.
	" if we are in math mode or if we do not check for it.
	if g:atp_no_math_command_completion != 1 &&  ( !g:atp_MathOpened || math_is_opened )
	    call extend(completion_list,g:atp_math_commands)
	    " amsmath && amssymb {{{5
	    " if g:atp_amsmath is set or the document class is ams...
	    if (g:atp_amsmath != 0 || atplib#DocumentClass(b:atp_MainFile) =~ '^ams')
		call extend(completion_list, g:atp_amsmath_commands,0)
		call extend(completion_list, g:atp_ams_negations)
		call extend(completion_list, g:atp_amsfonts)
		call extend(completion_list, g:atp_amsextra_commands)
		if a:expert_mode == 0 
		    call extend(completion_list, g:atp_ams_negations_non_expert_mode)
		endif
	    " else check if the packages are declared:
	    else
		if atplib#SearchPackage('amsmath', stop_line)
		    call extend(completion_list, g:atp_amsmath_commands,0)
		endif
		if atplib#SearchPackage('amssymb', stop_line)
		    call extend(completion_list, g:atp_ams_negations)
		    if a:expert_mode == 0 
			call extend(completion_list, g:atp_ams_negations_non_expert_mode)
		    endif
		endif
	    endif
	    " MathTools commands {{{5
	    if atplib#SearchPackage('mathtools', stop_line)
		call extend(completion_list, g:atp_MathTools_math_commands)
	    endif
	    call extend(completion_list, g:atp_math_commands_PRE, 0)
	
	    " nicefrac {{{5
	    if atplib#SearchPackage('nicefrac', stop_line)
		call add(completion_list,"\\nicefrac")
	    endif
	    " SIunits {{{5
	    if atplib#SearchPackage('SIunits', stop_line) && ( index(g:atp_completion_active_modes, 'SIunits') != -1 || index(g:atp_completion_active_modes, 'siunits') != -1 )
		call extend(completion_list, g:atp_siuinits)
	    endif
	    " math non expert mode {{{5
	    if a:expert_mode == 0
		call extend(completion_list, g:atp_math_commands_non_expert_mode)
	    endif
	endif
	" {{{4 -------------------- BEAMER commands
	if atplib#DocumentClass(b:atp_MainFile) == 'beamer'
	    call extend(completion_list, g:atp_BeamerCommands)
	endif
	" -------------------- LOCAL commands {{{4
	if g:atp_local_completion
	    " make a list of local envs and commands:
	    if !exists("b:atp_LocalCommands") 
		LocalCommands
	    elseif has("python")
		LocalCommands
	    endif
	    call extend(completion_list, b:atp_LocalCommands)
	endif
	" {{{4 -------------------- TIKZ commands
	" if tikz is declared and we are in tikz environment.
	let in_tikz=searchpair('\\begin\s*{tikzpicture}','','\\end\s*{tikzpicture}','bnW',"", max([1,(line(".")-g:atp_completion_limits[2])])) || atplib#CheckOpened('\\tikz{','}',line("."),g:atp_completion_limits[0])

	if in_tikz
	    " find all tikz libraries at once:
	    let tikz_libraries	= atplib#GrepPackageList('\\use\%(tikz\|pgf\)library\s*{')

	    " add every set of library commands:
	    for lib in tikz_libraries  
		if exists("g:atp_tikz_library_".lib."_commands")
		    call extend(completion_list, g:atp_tikz_library_{lib}_commands)
		endif   
	    endfor

	    " add common tikz commands:
	    call extend(completion_list, g:atp_tikz_commands)

	    " if in text mode add normal commands:
	    if searchpair('\\\@<!{', '', '\\\@<!}', 'bnW', "", max([ 1, (line(".")-g:atp_completion_limits[0])]))
		call extend(completion_list, g:atp_Commands)
	    endif
	endif 
	" {{{4 -------------------- COMMANDS
"	if we are not in math mode or if we do not care about it or we are in non expert mode.
	if (!g:atp_MathOpened || !math_is_opened ) || a:expert_mode == 0
	    call extend(completion_list,g:atp_Commands)
	    " FANCYHDR
	    if atplib#SearchPackage('fancyhdr', stop_line)
		call extend(completion_list, g:atp_fancyhdr_commands)
	    endif
	    if atplib#SearchPackage('makeidx', stop_line)
		call extend(completion_list, g:atp_makeidx_commands)
	    endif
	endif
	" {{{4 -------------------- MathTools commands
	if atplib#SearchPackage('mathtools', stop_line)
	    call extend(completion_list, g:atp_MathTools_commands)
	endif
	" {{{4 -------------------- ToDoNotes package commands
	if ( index(g:atp_completion_active_modes, 'todonotes') != -1 ) && atplib#SearchPackage('todonotes', stop_line)
	    call extend(completion_list, g:atp_TodoNotes_commands)
	endif
	"}}}4 
	" ToDo: add layout commands and many more packages. (COMMANDS FOR
	" PREAMBULE)
	"{{{4 -------------------- final stuff
	let env_name=substitute(pline,'.*\%(\\\%(begin\|end.*\){\(.\{-}\)}.*\|\\\%(\(item\)\s*\)\%(\[.*\]\)\?\s*$\)','\1\2','') 
	if env_name =~ '\\\%(\%(sub\)\?paragraph\|\%(sub\)*section\|chapter\|part\)'
	    let env_name=substitute(env_name,'.*\\\(\%(sub\)\?paragraph\|\%(sub\)*section\|chapter\|part\).*','\1','')
	endif
	let env_name=substitute(env_name,'\*$','','')
	" if the pattern did not work do not put the env name.
	" for example \item cos\lab<Tab> the pattern will not work and we do
	" not want env name. 
	if env_name == pline
	    let env_name=''
	endif

	if has_key(g:atp_shortname_dict,env_name)
	    if g:atp_shortname_dict[env_name] != 'no_short_name' && g:atp_shortname_dict[env_name] != '' 
		let short_env_name=g:atp_shortname_dict[env_name]
		let no_separator=0
	    else
		let short_env_name=''
		let no_separator=1
	    endif
	else
	    let short_env_name=''
	    let no_separator=1
	endif

" 	if index(g:atp_no_separator_list, env_name) != -1
" 	    let no_separator = 1
" 	endif

	if g:atp_env_short_names == 1
	    if no_separator == 0 && g:atp_no_separator == 0
		let short_env_name=short_env_name . g:atp_separator
	    endif
	else
	    let short_env_name=''
	endif

	call extend(completion_list, [ '\label{' . short_env_name ],0)
    " {{{3 ------------ ABBREVIATIONS
    elseif completion_method == 'abbreviations'
	let completion_list = [ "=document=","=description=","=letter=","=picture=","=list=","=minipage=","=titlepage=","=thebibliography=","=bibliography=","=center=","=flushright=","=flushleft=","=tikzpicture=","=frame=","=itemize=","=enumerate=","=quote=","=quotation=","=verse=","=abstract=","=verbatim=","=figure=","=array=","=table=","=tabular=","=equation=","=equation*=","=align=","=align*=","=alignat=","=alignat*=","=gather=","=gather*=","=multline=","=multline*=","=split=","=flalign=","=flalign*=","=corollary=","=theorem=","=proposition=","=lemma=","=definition=","=proof=","=remark=","=example=","=exercise=","=note=","=question=","=notation="]
	for env in reverse(b:atp_LocalEnvironments)
	    if index(completion_list, "=".env."=") == -1
		call extend(completion_list, ['='.env.'='],0)
	    endif
	endfor
    " {{{3 ------------ LABELS /are done later only the completions variable /
    elseif completion_method ==  'labels'
	let completion_list = []
    " {{{3 ------------ TEX INPUTFILES
    elseif completion_method ==  'inputfiles'
	let completion_list=[]
	call  extend(completion_list, atplib#KpsewhichGlobPath('tex', b:atp_OutDir . ',' . g:atp_texinputs, '*.tex', ':t:r', '^\%(\/home\|\.\|.*users\)', '\%(^\\usr\|texlive\|miktex\|kpsewhich\|generic\)'))
	call sort(completion_list)
    " {{{3 ------------ BIBFILES
    elseif completion_method ==  'bibfiles'
	let  completion_list=[]
	call extend(completion_list, atplib#KpsewhichGlobPath('bib', b:atp_OutDir . ',' . g:atp_bibinputs, '*.bib', ':t:r', '^\%(\/home\|\.\|.*users\)', '\%(^\\usr\|texlive\|miktex\|kpsewhich\|generic\|miktex\)'))
	call sort(completion_list)
    " {{{3 ------------ BIBSTYLES
    elseif completion_method == 'bibstyles'
	let completion_list=atplib#KpsewhichGlobPath("bst", "", "*.bst")
    "{{{3 ------------ DOCUMENTCLASS
    elseif completion_method == 'documentclass'
	if exists("g:atp_LatexClasses")
	    let completion_list	= copy(g:atp_LatexClasses)
	else
	    if g:atp_debugTabCompletion
		let debugTabCompletion_LatexClasses_TimeStart=reltime()
	    endif
	    let g:atp_LatexClasses	= atplib#KpsewhichGlobPath("tex", "", "*.cls")
	    if g:atp_debugTabCompletion
		let g:debugTabCompletion_LatexClasses_Time=reltimestr(reltime(debugTabCompletion_LatexClasses_TimeStart))
		call atplib#Log("TabCompletion.log", "LatexClasses Time: ".g:debugTabCompletion_LatexClasses_Time)
	    endif
	    let completion_list		= deepcopy(g:atp_LatexClasses)
	endif
	" \documentclass must be closed right after the name ends:
	if nchar != "}"
	    call map(completion_list,'v:val."}"')
	endif
    "{{{3 ------------ Beamer Themes
    elseif completion_method == 'beamerthemes'
	let completion_list = g:atp_BeamerThemes
    "{{{3 ------------ Beamer Inner Themes
    elseif completion_method == 'beamerinnerthemes'
	let completion_list = g:atp_BeamerInnerThemes
    "{{{3 ------------ Beamer Outer Themes
    elseif completion_method == 'beamerouterthemes'
	let completion_list = g:atp_BeamerOuterThemes
    "{{{3 ------------ Beamer Color Themes
    elseif completion_method == 'beamercolorthemes'
	let completion_list = g:atp_BeamerColorThemes
    "{{{3 ------------ Beamer Font Themes
    elseif completion_method == 'beamerfontthemes'
	let completion_list = g:atp_BeamerFontThemes
    "{{{3 ------------ FONT FAMILY
    elseif completion_method == 'font family'
	let bpos=searchpos('\\selectfon\zst','bnW',line("."))[1]
	let epos=searchpos('\\selectfont','nW',line("."))[1]-1
	if epos == -1
	    let epos=len(line)
	endif
	let fline=strpart(line,bpos,epos-bpos)
	let encoding=matchstr(fline,'\\\%(usefont\|DeclareFixedFont\s*{[^}]*}\|fontencoding\)\s*{\zs[^}]*\ze}')
	if encoding == ""
	    let encoding=g:atp_font_encoding
	endif
" 	    let g:encoding=encoding
	let completion_list=[]
	let fd_list=atplib#FdSearch('^'.encoding.begin)
" 	    let g:fd_list=copy(fd_list)
" 	    let g:begin = begin
	for file in fd_list
	    call extend(completion_list,map(atplib#ShowFonts(file),'matchstr(v:val,"usefont\\s*{[^}]*}\\s*{\\zs[^}]*\\ze}")'))
	endfor
	call filter(completion_list,'count(completion_list,v:val) == 1 ')
    "{{{3 ------------ FONT SERIES
    elseif completion_method == 'font series'
	let bpos=searchpos('\\selectfon\zst','bnW',line("."))[1]
	let epos=searchpos('\\selectfont','nW',line("."))[1]-1
	if epos == -1
	    let epos=len(line)
	endif
	let fline=strpart(line,bpos,epos-bpos)
" 	    let g:fline=fline
	let encoding=matchstr(fline,'\\\%(usefont\|DeclareFixedFont\s*{[^}]*}\|fontencoding\)\s*{\zs[^}]*\ze}')
	if encoding == ""
	    let encoding=g:atp_font_encoding
	endif
" 	    let g:encoding = encoding 
	let font_family=matchstr(fline,'\\\%(usefont\s*{[^}]*}\|DeclareFixedFont\s*{[^}]*}\s*{[^}]*}\|fontfamily\)\s*{\zs[^}]*\ze}')
" 	    let g:font_family=font_family
	let completion_list=[]
	let fd_list=atplib#FdSearch(encoding.font_family)
" 	    let g:fd_list = fd_list 
	for file in fd_list
	    call extend(completion_list,map(atplib#ShowFonts(file),'matchstr(v:val,"usefont{[^}]*}{[^}]*}{\\zs[^}]*\\ze}")'))
	endfor
	call filter(completion_list,'count(completion_list,v:val) == 1 ')
    "{{{3 ------------ FONT SHAPE
    elseif completion_method == 'font shape'
	let bpos=searchpos('\\selectfon\zst','bnW',line("."))[1]
	let epos=searchpos('\\selectfont','nW',line("."))[1]-1
	if epos == -1
	    let epos=len(line)
	endif
	let fline=strpart(line,bpos,epos-bpos)
	let encoding=matchstr(fline,'\\\%(usefont\|DeclareFixedFont\s*{[^}]*}\|fontencoding\)\s*{\zs[^}]*\ze}')
	if encoding == ""
	    let encoding=g:atp_font_encoding
	endif
	let font_family=matchstr(fline,'\\\%(usefont{[^}]*}\|DeclareFixedFont\s*{[^}]*}\s*{[^}]*}\|fontfamily\)\s*{\zs[^}]*\ze}')
	let font_series=matchstr(fline,'\\\%(usefont\s*{[^}]*}\s*{[^}]*}\|DeclareFixedFont\s*{[^}]*}\s*{[^}]*}\s*{[^}]*}\|fontseries\)\s*{\zs[^}]*\ze}')
	let completion_list=[]
	let fd_list=atplib#FdSearch('^'.encoding.font_family)

	for file in fd_list
	    call extend(completion_list,map(atplib#ShowFonts(file),'matchstr(v:val,"usefont{[^}]*}{'.font_family.'}{'.font_series.'}{\\zs[^}]*\\ze}")'))
	endfor
	call filter(completion_list,'count(completion_list,v:val) == 1 ')
    " {{{3 ------------ FONT ENCODING
    elseif completion_method == 'font encoding'
	let bpos=searchpos('\\selectfon\zst','bnW',line("."))[1]
	let epos=searchpos('\\selectfont','nW',line("."))[1]-1
	if epos == -1
	    let epos=len(line)
	endif
	let fline=strpart(line,bpos,epos-bpos)
	let font_family=matchstr(fline,'\\\%(usefont\s*{[^}]*}\|DeclareFixedFont\s*{[^}]*}\s*{[^}]*}\|fontfamily\)\s*{\zs[^}]*\ze}')
	if font_family != ""
	    let fd_list=atplib#FdSearch(font_family)
	    let completion_list=map(copy(fd_list),'toupper(substitute(fnamemodify(v:val,":t"),"'.font_family.'.*$","",""))')
	else
" 	    let completion_list=[]
" 	    for fd_file in fd_list
" 		let enc=substitute(fnamemodify(fd_file,":t"),"\\d\\zs.*$","","")
" 		if enc != fnamemodify(fd_file,":t")
" 		    call add(completion_list,toupper(enc))
" 		endif
" 	    endfor
	    let completion_list=g:atp_completion_font_encodings
	endif
    " {{{3 ------------ BIBITEMS
    elseif completion_method == 'bibitems'
	let time_bibitems=reltime()
	let col = col('.') - 1
	while col > 0 && line[col - 1] !~ '{\|,'
		let col -= 1
	endwhile
" 	let pat = ( strpart(l,col) == "" ? '.*' : strpart(l,col) )
	let pat = strpart(l,col)
	let searchbib_time=reltime()
	if len(filter(values(copy(b:TypeDict)), "v:val == 'bib'"))
	    if !exists("b:ListOfFiles") && !exists("b:TypeDict")
		call TreeOfFiles(b:atp_MainFile)
	    endif
	    if has("python") && g:atp_bibsearch == "python" && pat != ""
		let bibfiles=[]
		for f in b:ListOfFiles
		    if b:TypeDict[f] == 'bib'
			call add(bibfiles, f)
		    endif
		endfor
		let bibitems_list=values(atplib#searchbib_py(pat, bibfiles))
	    else
		let bibdict={}
		for f in b:ListOfFiles
		    if b:TypeDict[f] == 'bib'
			let bibdict[f]=readfile(f)
		    endif
		endfor
		let bibitems_list=values(atplib#searchbib(pat, bibdict))
	    endif
	    let g:bibitems_list=bibitems_list
	    let g:time_searchbib_py=reltimestr(reltime(searchbib_time))
	    if g:atp_debugTabCompletion
		let g:pat = pat
	    endif
	    let pre_completion_list=[]
	    let completion_dict=[]
	    let completion_list=[]
	    let time_bibitems_for=reltime()
	    for dict in bibitems_list
		for key in keys(dict)
		    " ToDo: change dict[key][...] to get() to not get errors
		    " if it is not present or to handle situations when it is not
		    " present!
		    call add(pre_completion_list, dict[key]['bibfield_key']) 
		    let bibkey=dict[key]['bibfield_key']
		    let bibkey=substitute(strpart(bibkey,max([stridx(bibkey,'{'),stridx(bibkey,'(')])+1),',\s*','','')
		    if nchar != ',' && nchar != '}'
			let bibkey.="}"
		    endif
		    let title=get(dict[key],'title','notitle')
		    let title=substitute(matchstr(title,'^\s*title\s*=\s*\%("\|{\|(\)\zs.*\ze\%("\|}\|)\)\s*\%(,\|$\)'),'{\|}','','g')
		    let year=get(dict[key],'year',"")
		    let year=matchstr(year,'^\s*year\s*=\s*\%("\|{\|(\)\zs.*\ze\%("\|}\|)\)\s*\%(,\|$\)')
		    let abbr=get(dict[key],'author',"noauthor")
		    let author = matchstr(abbr,'^\s*author\s*=\s*\%("\|{\|(\)\zs.*\ze\%("\|}\|)\)\s*,')
		    if abbr=="noauthor" || abbr == ""
			let abbr=get(dict[key],'editor',"")
			let author = matchstr(abbr,'^\s*editor\s*=\s*\%("\|{\|(\)\zs.*\ze\%("\|}\|)\)\s*,')
		    endif
		    if len(author) >= 40
			if match(author,'\sand\s')
			    let author=strpart(author,0,match(author,'\sand\s')) . ' et al.'
			else
			    let author=strpart(author,0,40)
			endif
		    endif
		    let author=substitute(author,'{\|}','','g')
		    if dict[key]['bibfield_key'] =~ 'article'
			let type="[a]"
		    elseif dict[key]['bibfield_key'] =~ 'book\>'
			let type="[B]"
		    elseif dict[key]['bibfield_key'] =~ 'booklet'
			let type="[b]"
		    elseif  dict[key]['bibfield_key'] =~ 'proceedings\|conference'
			let type="[p]"
		    elseif dict[key]['bibfield_key'] =~ 'unpublished'
			let type="[u]"
		    elseif dict[key]['bibfield_key'] =~ 'incollection'
			let type="[c]"
		    elseif dict[key]['bibfield_key'] =~ 'phdthesis'
			let type="[PhD]"
		    elseif dict[key]['bibfield_key'] =~ 'masterthesis'
			let type="[M]"
		    elseif dict[key]['bibfield_key'] =~ 'misc'
			let type="[-]"
		    elseif dict[key]['bibfield_key'] =~ 'techreport'
			let type="[t]"
		    elseif dict[key]['bibfield_key'] =~ 'manual'
			let type="[m]"
		    else
			let type="   "
		    endif

		    let abbr=type." ".author." (".year.") "

		    call add(completion_dict, { "word" : bibkey, "menu" : title, "abbr" : abbr }) 
		endfor
	    endfor
            let g:completion_dict=completion_dict
	    for key in pre_completion_list
		call add(completion_list,substitute(strpart(key,max([stridx(key,'{'),stridx(key,'(')])+1),',\s*','',''))
	    endfor
	else
	    " add the \bibitems found in include files
	    let time_bibitems_SearchBibItems=reltime()
            let completion_dict=[]
            let dict=atplib#SearchBibItems()
            for key in keys(dict)
                call add(completion_dict, { "word" : key, "menu" : dict[key]['rest'], "abbrev" : dict[key]['label'] })
            endfor
	    let g:time_bibitems_SearchBibItems=reltimestr(reltime(time_bibitems_SearchBibItems))
	endif
	let g:time_bibitems=reltimestr(reltime(time_bibitems))
    " {{{3 ------------ TodoNotes todo & missing figure options
    elseif completion_method == 'todo options'
	let completion_list = g:atp_TodoNotes_todo_options
    elseif completion_method == 'missingfigure options'
	let completion_list = g:atp_TodoNotes_missingfigure_options
    " {{{3 ------------ Colors
    elseif completion_method == 'colors'
	" ToDo:
	let completion_list=[]
    endif
    " }}}3
    if exists("completion_list")
	let b:completion_list=completion_list	" DEBUG
    endif
" {{{2 make the list of matching completions
    "{{{3 --------- completion_method = !close environments !env_close
    if completion_method != 'close environments' && completion_method != 'env_close'
	let completions=[]
	    " {{{4 --------- packages, bibstyles, font (family, series, shapre, encoding), document class
	    if (completion_method == 'package' 		||
			\ completion_method == 'bibstyles' 	||
			\ completion_method =~ 'beamer\%(\|inner\|outer\|color\|font\)themes' ||
			\ completion_method == 'font family' 	||
			\ completion_method == 'font series' 	||
			\ completion_method == 'font shape'	||
			\ completion_method == 'font encoding'||
			\ completion_method == 'pagestyle'||
			\ completion_method == 'pagenumbering'||
			\ completion_method == 'documentclass' )
		if a:expert_mode
		    let completions	= filter(deepcopy(completion_list),' v:val =~? "^".begin') 
		else
		    let completions	= filter(deepcopy(completion_list),' v:val =~? begin') 
		endif
	    " {{{4 --------- environment names, bibfiles 
	    elseif ( completion_method == 'environment_names'	||
			\ completion_method == 'bibfiles' 	)
		if a:expert_mode
		    let completions	= filter(deepcopy(completion_list),' v:val =~# "^".begin') 
		else
		    let completions	= filter(deepcopy(completion_list),' v:val =~? begin') 
		endif
	    " {{{4 --------- colors
	    elseif completion_method == 'colors'
		if a:expert_mode
		    let completions	= filter(deepcopy(completion_list),' v:val =~# "^".color_begin') 
		else
		    let completions	= filter(deepcopy(completion_list),' v:val =~? color_begin') 
		endif
	    " {{{4 --------- tikz libraries, inputfiles 
	    " match not only in the beginning
	    elseif (completion_method == 'tikz libraries' ||
			\ completion_method == 'inputfiles')
		let completions	= filter(deepcopy(completion_list),' v:val =~? begin') 
		if nchar != "}" && nchar != "," && completion_method != 'inputfiles'
		    call map(completions,'v:val')
		endif
	    " {{{4 --------- Commands 
	    " must match at the beginning (but in a different way)
	    " (only in expert_mode).
	    elseif completion_method == 'command' 
			if a:expert_mode == 1 
			    let completions	= filter(copy(completion_list),'v:val =~# "\\\\".tbegin')
			elseif a:expert_mode != 1 
			    let completions	= filter(copy(completion_list),'v:val =~? tbegin')
			endif
	    " {{{4 --------- Abbreviations
	    elseif completion_method == 'abbreviations'
		let completions		= filter(copy(completion_list), 'v:val =~# "^" . abegin')
	    " {{{4 --------- Tikzpicture Keywords
	    elseif completion_method == 'tikzpicture keywords' || 
			\ completion_method == 'todo options' ||
			\ completion_method == 'missingfigure options'
		if a:expert_mode == 1 
		    let completions	= filter(deepcopy(completion_list),'v:val =~# "^".tbegin') 
		elseif a:expert_mode != 1 
		    let completions	= filter(deepcopy(completion_list),'v:val =~? tbegin') 
		endif
	    " {{{4 --------- Tikzpicture Commands
	    elseif completion_method == 'tikzpicture commands'
		if a:expert_mode == 1 
		    let completions	= filter(deepcopy(completion_list),'v:val =~# "^".tbegin') 
		elseif a:expert_mode != 1 
		    let completions	= filter(deepcopy(completion_list),'v:val =~? tbegin') 
		endif
	    " {{{4 --------- Labels
	    elseif completion_method == 'labels'
		" Complete label by string or number:
		
		let aux_data	= atplib#GrepAuxFile()
		let completion_dict = []
		let pattern 	= matchstr(l, '\%(.\|\\\%(eq\)\=ref\)*\\\%(eq\)\=ref\s*{\zs\S*$')
		for data in aux_data
		    " match label by string or number
		    if ( data[0] =~ '^' . pattern || data[1] =~ '^'. pattern . '$' ) && a:expert_mode || ( data[0] =~ pattern || data[1] =~ '^'. pattern ) && !a:expert_mode
			let close = ( nchar == '}' ? '' : '}' )
			call add(completion_dict, { "word" : data[0].close, "abbr" : data[0], "menu" : ( data[2] == 'equation' && data[1] != "" ? "(".data[1].")" : data[1] ) , "kind" : data[2][0] })
		    endif
		endfor 
	    endif
    "{{{3 --------- else: try to close environment
    else
	call atplib#CloseLastEnvironment('a', 'environment')
	let b:tc_return="1"
	return ''
    endif
    "{{{3 --------- SORTING and TRUNCATION
    " ToDo: we will not truncate if completion method is specific, this should be
    " made by a variable! Maybe better is to provide a positive list !!!
    if g:atp_completion_truncate && a:expert_mode && 
		\ index(['bibfiles', 'bibitems', 'bibstyles', 'font family',
		\ 'font series', 'font shape', 'font encoding' ],completion_method) == -1
	call filter(completions,'len(substitute(v:val,"^\\","","")) >= g:atp_completion_truncate')
    endif
"     THINK: about this ...
"     if completion_method == "tikzpicture keywords"
" 	let bracket	= atplib#CloseLastBracket(g:atp_bracket_dict, 1)
" 	if bracket != ""
" 	    call add(completions, bracket)
" 	endif
"     endif
    " if the list is long it is better if it is sorted, if it short it is
    " better if the more used things are at the beginning.
    if g:atp_sort_completion_list && len(completions) >= g:atp_sort_completion_list && completion_method != 'labels'
	let completions=sort(completions)
    endif
    " DEBUG
    let b:completions=completions 
    " {{{2 COMPLETE 
    " {{{3 package, tikz libraries, environment_names, colors, bibfiles, bibstyles, documentclass, font family, font series, font shape font encoding and input files 
    if
		\ completion_method == 'package' 	|| 
		\ completion_method == 'tikz libraries'    || 
		\ completion_method == 'environment_names' ||
		\ completion_method == 'abbreviations' ||
		\ completion_method == 'pagestyle'	||
		\ completion_method == 'pagenumbering'	||
		\ completion_method == 'bibfiles' 	|| 
		\ completion_method == 'bibstyles' 	|| 
		\ completion_method == 'documentclass'|| 
		\ completion_method =~ 'beamer\%(\|inner\|outer\|color\|font\)themes' ||
		\ completion_method == 'font family'  ||
		\ completion_method == 'font series'  ||
		\ completion_method == 'font shape'   ||
		\ completion_method == 'font encoding'||
		\ completion_method == 'todo options' ||
		\ completion_method == 'missingfigure options' ||
		\ completion_method == 'inputfiles' 
	call complete(nr+2,completions)
    "{{{3 labels
    elseif completion_method == 'labels'
	let col=match(l, '\%(.\|\\\%(eq\)\=ref\)*\\\(eq\)\=ref\s*{\zs\S*$')+1
	call complete(col, completion_dict)
    "{{{3 colors
    elseif completion_method == 'colors'
	call complete(color_nr+2,completions)
    " {{{3 bibitems
    elseif !normal_mode && completion_method == 'bibitems'
        if exists("completion_dict")
            " for bibtex, biblatex
            call complete(col+1,completion_dict)
        else
            " for thebibliography environment
            call complete(col+1,completion_list)
        endif
    " {{{3 command, tikzcpicture commands
    elseif !normal_mode && (completion_method == 'command' || completion_method == 'tikzpicture commands')
	call complete(o+1,completions)
	let b:tc_return="command X"
    " {{{3 tikzpicture keywords
    elseif !normal_mode && (completion_method == 'tikzpicture keywords')
	let t=match(l,'\zs\<\w*$')
	" in case '\zs\<\w*$ is empty
	if t == -1
	    let t=col(".")
	endif
	call complete(t+1,completions)
	let b:tc_return="tikzpicture keywords"
    endif
    " If the completion method was a command (probably in a math mode) and
    " there was no completion, check if environments are closed.
    " {{{ 3 Final call of CloseLastEnvrionment / CloseLastBracket
    let len=len(completions)
    if len == 0 && (!count(['package', 'bibfiles', 'bibstyles', 'inputfiles'], completion_method) || a:expert_mode == 1 )|| len == 1
	if count(['command', 'tikzpicture commands', 'tikzpicture keywords'], completion_method) && 
	    \ (len == 0 || len == 1 && completions[0] =~ '^\\\='. begin . '$' )

	    let filter 		= 'strpart(getline("."), 0, col(".") - 1) =~ ''\\\@<!%'''
	    let stopline 	= search('^\s*$\|\\par\>', 'bnW')

	    " Check Brackets 
	    let cl_return 	= atplib#CloseLastBracket(g:atp_bracket_dict)
	    if g:atp_debugTabCompletion
		let g:return	= cl_return
	    endif
	    " If the bracket was closed return
	    if cl_return != "0"
		return ""
	    endif

	    " Check inline math:
	    if atplib#CheckInlineMath('texMathZoneV') || 
			\ atplib#CheckInlineMath('texMathZoneW') ||
			\ atplib#CheckInlineMath('texMathZoneX') ||
			\ b:atp_TexFlavor == 'plaintex' && atplib#CheckInlineMath('texMathZoneY')
		let zone = 'texMathZoneVWXY' 	" DEBUG
		call atplib#CloseLastEnvironment(append, 'math')

	    " Check environments:
	    else
		let env_opened= searchpairpos('\\begin','','\\end','bnW','searchpair("\\\\begin{".matchstr(getline("."),"\\\\begin{\\zs[^}]*\\ze}"),"","\\\\end{".matchstr(getline("."),"\\\\begin{\\zs[^}]*\\ze}"),"nW")',max([1,(line(".")-g:atp_completion_limits[2])]))
		let env_name 	= matchstr(strpart(getline(env_opened[0]), env_opened[1]-1), '\\begin\s*{\zs[^}]*\ze}')
		let zone	= env_name 	" DEBUG
		if env_opened != [0, 0]
		    call atplib#CloseLastEnvironment('a', 'environment', env_name, env_opened)
		endif
	    endif
	    " DEBUG
	    if exists("zone")
		let b:tc_return.=" close_env end " . zone
		let b:comp_method.=' close_env end ' . zone
		call atplib#Log("TabCompletion.log", "b:comp_method.=".b:comp_method)
	    else
		let b:tc_return.=" close_env end"
		let b:comp_method.=' close_env end'
		call atplib#Log("TabCompletion.log", "b:comp_method=".b:comp_method)
	    endif
	elseif completion_method == 'package' || 
		    \  completion_method == 'bibstyles' || 
		    \ completion_method == 'bibfiles'
	    let b:tc_return='close_bracket end'
	    call atplib#CloseLastBracket(g:atp_bracket_dict)
	endif
    endif
    "}}}3
""}}}2
"  ToDo: (a challenging one)  
"  Move one step after completion is done (see the condition).
"  for this one have to end till complete() function will end, and this can be
"  done using (g)vim server functions.
"     let b:check=0
"     if completion_method == 'environment_names' && end =~ '\s*}'
" 	let b:check=1
" 	let pos=getpos(".")
" 	let pos[2]+=1
" 	call setpos(".",pos) 
"     endif
"
    " unlet variables if there were defined.
    if exists("completion_list")
	unlet completion_list
    endif
    if exists("completions")
	unlet completions
    endif
    return ''
    "}}}2
endfunction
catch /E127:/
endtry
" }}}1

" Font Preview Functions:
"{{{1 Font Preview Functions
" These functions search for fd files and show them in a buffer with filetype
" 'fd_atp'. There are additional function for this filetype written in
" fd_atp.vim ftplugin. Distributed with atp.
"{{{2 atplib#FdSearch
"([<pattern>,<method>])
" There are two methods: 
" 	0 - match fd file names ( ":t" filename modifier)        <- the default one
" 	1 - match fd file path 
function! atplib#FdSearch(...)

    if a:0 == 0
	let pattern	= ""
	let method	= 0
    else
	let pattern	= ( a:0 >= 1 ? a:1 : "" )
	let method	= ( a:0 >= 2 ? a:2 : 0 )
    endif

"     let g:method = method
"     let g:pattern = pattern

    " Find fd file
    let path	= substitute(substitute(system("kpsewhich -show-path tex"),'!!','','g'),'\/\/\+','\/','g')
    let path	= substitute(path,':\|\n',',','g')
    let fd 	= split(globpath(path,"**/*.fd"),'\n') 

"     let g:fd	= copy(fd)

    " Match for l:pattern
    let fd_matches=[]
    if method == 0
	call filter(fd, 'fnamemodify(v:val, ":t") =~ pattern') 
    else
	call filter(fd, 'v:val =~ pattern') 
    endif

    return fd
endfunction
"{{{2 atplib#FontSearch
" atplib#FontSearch(method,[<pattern>]) 
" method = "" match for name of fd file
" method = "!" match against whole path
if !exists("*atplib#FontSearch")
function! atplib#FontSearch(method,...)
	
    let l:method	= ( a:method == "!" ? 1 : 0 )
    let l:pattern	= ( a:0 ? a:1 : "" )

    let s:fd_matches=atplib#FdSearch(l:pattern, l:method)

    " Open Buffer and list fd files
    " set filetype to fd_atp
    let l:tmp_dir=tempname()
    call mkdir(l:tmp_dir)
    let l:fd_bufname="fd_list " . l:pattern
    let l:openbuffer="32vsplit! +setl\\ nospell\\ ft=fd_atp ". fnameescape(l:tmp_dir . "/" . l:fd_bufname )

    let g:fd_matches=[]
    if len(s:fd_matches) > 0
	echohl WarningMsg
	echomsg "[ATP:] found " . len(s:fd_matches) . " files."
	echohl None
	" wipe out the old buffer and open new one instead
	if buflisted(fnameescape(l:tmp_dir . "/" . l:fd_bufname))
	    silent exe "bd! " . bufnr(fnameescape(l:tmp_dir . "/" . l:fd_bufname))
	endif
	silent exe l:openbuffer
	" make l:tmp_dir available for this buffer.
" 	let b:tmp_dir=l:tmp_dir
	cd /tmp
	map <buffer> q	:bd<CR>

	" print the lines into the buffer
	let l:i=0
	call setline(1,"FONT DEFINITION FILES:")
	for l:fd_file in s:fd_matches
	    " we put in line the last directory/fd_filename:
	    " this is what we cut:
	    let l:path=fnamemodify(l:fd_file,":h:h")
	    let l:fd_name=substitute(l:fd_file,"^" . l:path . '/\?','','')
" 	    call setline(line('$')+1,fnamemodify(l:fd_file,":t"))
	    call setline(line('$')+1,l:fd_name)
	    call add(g:fd_matches,l:fd_file)
	    let l:i+=1
	endfor
	call append('$', ['', 'maps:', 
			\ 'p       Preview font ', 
			\ 'P       Preview font+tex file', 
			\ '<Tab>   Show Fonts in fd file', 
			\ '<Enter> Open fd file', 
			\ 'q       "bd!"',
			\ '',
			\ 'Note: p/P works in visual mode'])
	silent w
	setlocal nomodifiable
	setlocal ro
    else
	echohl WarningMsg
	if !l:method
	    echomsg "[ATP:] no fd file found, try :FontSearch!"
	else
	    echomsg "[ATP:] no fd file found."
	endif
	echohl None
    endif

endfunction
endif
"}}}2
"{{{2 atplib#Fd_completion /not needed/
" if !exists("*atplib#Fd_completion")
" function! atplib#Fd_completion(A,C,P)
"     	
"     " Find all files
"     let l:path=substitute(substitute(system("kpsewhich -show-path tex"),'!!','','g'),'\/\/\+','\/','g')
"     let l:path=substitute(l:path,':\|\n',',','g')
"     let l:fd=split(globpath(l:path,"**/*.fd"),'\n') 
"     let l:fd=map(l:fd,'fnamemodify(v:val,":t:r")')
" 
"     let l:matches=[]
"     for l:fd_file in l:fd
" 	if l:fd_file =~ a:A
" 	    call add(l:matches,l:fd_file)
" 	endif
"     endfor
"     return l:matches
" endfunction
" endif
" }}}2
" {{{2 atplib#OpenFdFile /not working && not needed?/
" function! atplib#OpenFdFile(name)
"     let l:path=substitute(substitute(system("kpsewhich -show-path tex"),'!!','','g'),'\/\/\+','\/','g')
"     let l:path=substitute(l:path,':\|\n',',','g')
"     let b:path=l:path
"     let l:fd=split(globpath(l:path,"**/".a:name.".fd"),'\n') 
"     let l:fd=map(l:fd,'fnamemodify(v:val,":t:r")')
"     let b:fd=l:fd
"     execute "split +setl\\ ft=fd_atp " . l:fd[0]
" endfunction
" }}}2
"{{{2 atplib#Preview
" keep_tex=1 open the tex file of the sample file, otherwise it is deleted (at
" least from the buffer list).
" To Do: fd_file could be a list of fd_files which we would like to see, every
" font should be done after \pagebreak[4]
" if a:fd_files=['buffer'] it means read the current buffer (if one has opened
" an fd file).
function! atplib#Preview(fd_files,keep_tex)
    if a:fd_files != ["buffer"]
	let l:fd_files={}
	for l:fd_file in a:fd_files
	    call extend(l:fd_files,{fd_file : readfile(l:fd_file)})
	endfor
    else
	let l:fd_files={bufname("%"):getline(1,"$")}
    endif
    unlet l:fd_file

    let l:declare_command='\C\%(DeclareFontShape\%(WithSizes\)\?\|sauter@\%(tt\)\?family\|EC@\%(tt\)\?family\|krntstexmplfamily\|HFO@\%(tt\)\?family\)'
    let b:declare_command=l:declare_command
    
    let l:font_decl_dict={}
    for l:fd_file in a:fd_files
	call extend(l:font_decl_dict, {l:fd_file : [ ]})
	for l:line in l:fd_files[l:fd_file]
	    if l:line =~ '\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'
		call add(l:font_decl_dict[l:fd_file],l:line)
	    endif
	endfor
    endfor

    if exists("b:tmp_dir")
	let l:tmp_dir=b:tmp_dir
    else
	let l:tmp_dir=tempname()
    endif
    if !isdirectory(l:tmp_dir)
	call mkdir(l:tmp_dir)
    endif
    if a:fd_files == ["buffer"]
	" WINDOWS NOT COMPATIBLE
	let l:testfont_file=l:tmp_dir . "/" . fnamemodify(bufname("%"),":t:r") . ".tex"
    else
	" the name could be taken from the pattern
	" or join(map(keys(deepcopy(a:fd_files)),'substitute(fnamemodify(v:val,":t:r"),".fd$","","")'),'_')
	" though it can be quite a long name.
	let l:testfont_file=l:tmp_dir . "/" . fnamemodify(a:fd_files[0],":t:r") . ".tex"
    endif
    " WINDOWS NOT COMPATIBLE
"     call system("touch " . l:testfont_file)
    
    let l:fd_bufnr=bufnr("%")

    let s:text="On November 14, 1885, Senator \\& Mrs.~Leland Stanford called
		\ together at their San Francisco mansion the 24~prominent men who had
		\ been chosen as the first trustees of The Leland Stanford Junior University.
		\ They handed to the board the Founding Grant of the University, which they
		\ had executed three days before.\\\\
		\ (!`THE DAZED BROWN FOX QUICKLY GAVE 12345--67890 JUMPS!)"

"     let l:text="On November 14, 1885, Senator \\& Mrs.~Leland Stanford called
" 	\ together at their San Francisco mansion the 24~prominent men who had
" 	\ been chosen as the first trustees of The Leland Stanford Junior University.
" 	\ They handed to the board the Founding Grant of the University, which they
" 	\ had executed three days before. This document---with various amendments,
" 	\ legislative acts, and court decrees---remains as the University's charter.
" 	\ In bold, sweeping language it stipulates that the objectives of the University
" 	\ are ``to qualify students for personal success and direct usefulness in life;
" 	\ and to promote the public welfare by exercising an influence in behalf of
" 	\ humanity and civilization, teaching the blessings of liberty regulated by
" 	\ law, and inculcating love and reverence for the great principles of
" 	\ government as derived from the inalienable rights of man to life, liberty,
" 	\ and the pursuit of happiness.''\\
" 	\ (!`THE DAZED BROWN FOX QUICKLY GAVE 12345--67890 JUMPS!)\\par}}
" 	\ \\def\\\moretext{?`But aren't Kafka's Schlo{\\ss} and {\\AE}sop's {\\OE}uvres
" 	\ often na{\\"\\i}ve  vis-\\`a-vis the d{\\ae}monic ph{\\oe}nix's official r\\^ole
" 	\ in fluffy souffl\\'es? }
" 	\ \\moretext"

    if a:fd_files == ["buffer"]
	let l:openbuffer="edit "
    else
	let l:openbuffer="topleft split!"
    endif
    execute l:openbuffer . " +setlocal\\ ft=tex\\ modifiable\\ noro " . l:testfont_file 
    let b:atp_ProjectScript = 0
    map <buffer> q :bd!<CR>

    call setline(1,'\documentclass{article}')
    call setline(2,'\oddsidemargin=0pt')
    call setline(3,'\textwidth=450pt')
    call setline(4,'\textheight=700pt')
    call setline(5,'\topmargin=-10pt')
    call setline(6,'\headsep=0pt')
    call setline(7,'\begin{document}')

    let l:i=8
    let l:j=1
    let l:len_font_decl_dict=len(l:font_decl_dict)
    let b:len_font_decl_dict=l:len_font_decl_dict
    for l:fd_file in keys(l:font_decl_dict) 
	if l:j == 1 
	    call setline(l:i,'\textsc\textbf{\Large Fonts from the file '.l:fd_file.'}\\[2em]')
	    let l:i+=1
	else
" 	    call setline(l:i,'\pagebreak[4]')
	    call setline(l:i,'\vspace{4em}')
	    call setline(l:i+1,'')
	    call setline(l:i+2,'\textsc\textbf{\Large Fonts from the file '.l:fd_file.'}\\[2em]')
	    let l:i+=3
	endif
	let l:len_font_decl=len(l:font_decl_dict[l:fd_file])
	let b:match=[]
	for l:font in l:font_decl_dict[l:fd_file]
	    " SHOW THE FONT ENCODING, FAMILY, SERIES and SHAPE
	    if matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') == "b" ||
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') == "bx"
		let b:show_font='\noindent{\large \textit{Font Encoding}: \textsf{' . 
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			    \ ' \textit{Font Family}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^}#]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			    \ ' \textit{Font Series}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') . '}' . 
			    \ ' \textit{Font Shape}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}') . '}}\\[2pt]'
	    else
		let b:show_font='\noindent{\large \textbf{Font Encoding}: \textsf{' . 
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			    \ ' \textbf{Font Family}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^}#]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}\s*{[^#}]*}') . '}' . 
			    \ ' \textbf{Font Series}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}\s*{[^#}]*}') . '}' . 
			    \ ' \textbf{Font Shape}: \textsf{' .  
			    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{\zs[^#}]*\ze}') . '}}\\[2pt]'
	    endif
	    call setline(l:i,b:show_font)
	    let l:i+=1
	    " CHANGE THE FONT
	    call setline(l:i,'{' . substitute(
			\ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'),
			\ l:declare_command,'usefont','') . 
			\ '\selectfont')
	    " WRITE SAMPLE TEXT
	    call add(b:match,matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'))
	    let l:i+=1
	    " END
	    if l:j<l:len_font_decl
		call setline(l:i,s:text . '}\\\\')
	    else
		call setline(l:i,s:text . '}')
	    endif
	    let l:i+=1
	    let l:j+=1
	endfor
    endfor
    call setline(l:i,'\end{document}')
    silent w
    if b:atp_TexCompiler =~ '^pdf'	
	let l:ext=".pdf"
    else
	let l:ext=".dvi"
    endif
    call system(b:atp_TexCompiler . " " . l:testfont_file . 
	    \ " && " . b:atp_Viewer . " " . fnamemodify(l:testfont_file,":p:r") . l:ext ." &")
    if !a:keep_tex
	silent exe "bd"
    endif
endfunction
" }}}2
"{{{2 atplib#FontPreview
" a:fd_file  pattern to find fd file (.fd will be appended if it is not
" present at the end),
" a:1 = encoding
" a:2 = l:keep_tex, i.e. show the tex source.
function! atplib#FontPreview(method, fd_file,...)


    let l:method	= ( a:method == "!" ? 1 : 0 )
    let l:enc		= ( a:0 >= 1 ? a:1 : "" )
    let l:keep_tex 	= ( a:0 >= 2 ? a:2 : 0 )

    if filereadable(a:fd_file)
	let l:fd_file=a:fd_file
    else
	" Find fd file
	if a:fd_file !~ '.fd\s*$'
	    let l:fd_file=a:fd_file.".*.fd"
	else
	    let l:fd_file=a:fd_file
	endif

	let l:fd=atplib#FdSearch(a:fd_file, l:method)
	let g:fd=l:fd
	if !empty(l:enc)
	    call filter(l:fd, "fnamemodify(v:val, ':t') =~ '^' . l:enc")
	endif

	if len(l:fd) == 0
	    if !l:method
		echo "FD file not found. Try :FontPreview!"
	    else
		echo "FD file not found."
	    endif
	    return
	elseif len(l:fd) == 1
	    let l:fd_file_list=l:fd
	else
	    let l:i=1
	    for l:f in l:fd
		echo l:i." ".substitute(f,'^'.fnamemodify(f,":h:h").'/\?','','')
		let l:i+=1
	    endfor
	    let l:choice=input('Which fd file? ')
	    if l:choice == "" 
		return
	    endif
	    let l:choice_list=split(l:choice,',')
	    let b:choice_list=l:choice_list
	    " if there is 1-4  --> a list of 1,2,3,4
	    let l:new_choice_list=[]
	    for l:ch in l:choice_list
		if l:ch =~ '^\d\+$'
		    call add(l:new_choice_list,l:ch)
		elseif l:ch =~ '^\d\+\s*-\s*\d\+$'
		    let l:b=matchstr(l:ch,'^\d\+')
		    let l:e=matchstr(l:ch,'\d\+$')
		    let l:k=l:b
		    while l:k<=l:e
			call add(l:new_choice_list,l:k)
			let l:k+=1
		    endwhile
		endif
	    endfor
	    let b:new_choice_list=l:new_choice_list
	    let l:fd_file_list=map(copy(l:new_choice_list),'get(l:fd,(v:val-1),"")')
	    let l:fd_file_list=filter(l:fd_file_list,'v:val != ""')
" 	    let l:fd_file=get(l:fd,l:choice-1,"return")
	    if len(l:fd_file_list) == 0
		return
	    endif
	endif
    endif
    call atplib#Preview(l:fd_file_list,l:keep_tex)
endfunction
"}}}2
" {{{2 atplib#ShowFonts
function! atplib#ShowFonts(fd_file)
    let l:declare_command='\C\%(DeclareFontShape\%(WithSizes\)\?\|sauter@\%(tt\)\?family\|EC@\%(tt\)\?family\|krntstexmplfamily\|HFO@\%(tt\)\?family\)'
    
    let l:font_decl=[]
    for l:line in readfile(a:fd_file)
	if l:line =~ '\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'
	    call add(l:font_decl,l:line)
	endif
    endfor
    let l:font_commands=[]
    for l:font in l:font_decl
	call add(l:font_commands,substitute(
		    \ matchstr(l:font,'\\'.l:declare_command.'\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}\s*{[^#}]*}'),
		    \ l:declare_command,'usefont',''))
    endfor
    return l:font_commands
endfunction
"}}}2
" }}}1

" vim:fdm=marker:ff=unix:noet:ts=8:sw=4:fdc=1
