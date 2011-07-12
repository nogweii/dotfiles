" Author:	Marcin Szamotulski
" Description:  This file provides searching tools of ATP.
" Note:		This file is a part of Automatic Tex Plugin for Vim.
" Language:	tex
" Last Change:

let s:sourced 	= exists("s:sourced") ? 1 : 0

" Functions: (soure once)
if !s:sourced || g:atp_reload_functions "{{{
" Make a dictionary of definitions found in all input files.
" {{{ s:make_defi_dict_vim
" Comparing with ]D, ]d, ]i, ]I vim maps this function deals with multiline
" definitions.
"
" The output dictionary is of the form: 
" 	{ input_file : [ [begin_line, end_line], ... ] }
" a:1 	= buffer name to search in for input files
" a:3	= 1 	skip searching for the end_line
"
" ToDo: it is possible to check for the end using searchpairpos, but it
" operates on a list not on a buffer.
function! s:make_defi_dict_vim(bang,...)

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    let bufname	= a:0 >= 1 ? a:1 : atp_MainFile

    " pattern to match the definitions this function is also used to fine
    " \newtheorem, and \newenvironment commands  
    let pattern	= a:0 >= 2 ? a:2 : '\\def\|\\newcommand'

    let preambule_only	= ( a:bang == "!" ? 0 : 1 )

    " this is still to slow!
    let only_begining	= ( a:0 >= 3 ? a:3 : 0 )

    let defi_dict={}

    let inputfiles=FindInputFiles(bufname)
    let input_files=[]

    " TeX: How this work in TeX files.
    for inputfile in keys(inputfiles)
	if inputfiles[inputfile][0] != "bib" && ( !preambule_only || inputfiles[inputfile][0] == "preambule" )
	    call add(input_files, inputfiles[inputfile][2])
	endif
    endfor

    let input_files=filter(input_files, 'v:val != ""')
    if !count(input_files, atp_MainFile)
	call extend(input_files,[ atp_MainFile ])
    endif

    if len(input_files) > 0
    for inputfile in input_files
	let defi_dict[inputfile]=[]
	" do not search for definitions in bib files 
	"TODO: it skips lines somehow. 
	let ifile=readfile(inputfile)
	
	" search for definitions
	let lnr=1
	while (lnr <= len(ifile) && (!preambule_only || ifile[lnr-1] !~ '\\begin\s*{document}'))

	    let match=0

	    let line=ifile[lnr-1]
	    if substitute(line,'\\\@<!%.*','','') =~ pattern

		let b_line=lnr

		let lnr+=1	
		if !only_begining
		    let open=atplib#count(line,'{')    
		    let close=atplib#count(line,'}')
		    while open != close
			"go to next line and count if the definition ends at
			"this line
			let line	= ifile[lnr-1]
			let open	+=atplib#count(line,'{')    
			let close	+=atplib#count(line,'}')
			let lnr		+=1	
		    endwhile
		    let e_line	= lnr-1
		    call add(defi_dict[inputfile], [ b_line, e_line ])
		else
		    call add(defi_dict[inputfile], [ b_line, b_line ])
		endif
	    else
		let lnr+=1
	    endif
	endwhile
    endfor
    endif

    return defi_dict
endfunction "}}}
" {{{ s:make_defi_dict_py
" command! -nargs=* -bang MakeDefiDict  :call s:make_defi_dict_py(<q-bang>,<f-args>)
function! s:make_defi_dict_py(bang,...)

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    let bufname	= a:0 >= 1 ? a:1 : atp_MainFile
    " Not tested
    let pattern	= a:0 >= 2 ? a:2 : '\\def\|\\newcommand'
    " Not implemeted
    let preambule_only= a:bang == "!" ? 0 : 1
    let g:preambule_only=preambule_only
    let only_begining	= a:0 >= 3 ? a:3 : 0
    let g:only_begining=only_begining

    if a:bang == "!" || !exists("b:TreeOfFiles")
	 " Update the cached values:
	 let [ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ] = TreeOfFiles(atp_MainFile)
    endif
    let [ Tree, List, Type_Dict, Level_Dict ] = deepcopy([ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ])
 
python << ENDPYTHON
import re, subprocess, os, glob

def preambule_end(file):
# find linenr where preambule ends,

# file is list of lines
    nr=1
    for line in file:
        if re.search('\\\\begin\s*{\s*document\s*}', line):
            return nr
        nr+=1
    return 0

pattern=vim.eval("pattern")
type_dict=vim.eval("b:TypeDict")
main_file=vim.eval("atp_MainFile")
if int(vim.eval("preambule_only")) != 0:
    preambule_only=True
    files=[main_file]
    for f in type_dict.keys():
        if type_dict[f] == "preambule":
            files.append(f)
    main_file_ob=open(main_file, 'r')
    main_file_l=main_file_ob.read().split("\n")
    main_file_ob.close()
    preambule_end=preambule_end(main_file_l)
else:
    preambule_only=False
    files=[main_file]
    files.extend(vim.eval("b:ListOfFiles"))
if vim.eval("only_begining") !=0:
    only_begining=True
else:
    only_begining=False

def isnonempty(string):
    if str(string) == "":
        return False
    else:
        return True

if pattern == "":
    pat=".*"
else:
    pat=pattern

# Does the no comment work?
pattern=re.compile('^(?:[^%]|\\\\%)*(?:\\\\def|\\\\(?:re)?newcommand\s*{|\\\\providecommand\s*{|\\\\(?:re)?newenvironment\s*{|\\\\(?:re)?newtheorem\s*{|\\\\definecolor\s*{)')

defi_dict={}
for file in files:
    defi_dict[file]=[]
    lnr=1
    file_ob=open(file, 'r')
    file_l=file_ob.read().split("\n")
    file_ob.close()
    while lnr <= len(file_l) and ( preambule_only and ( file == main_file and lnr <= preambule_end or file != main_file ) or not preambule_only):
        line=file_l[lnr-1]
        if re.search(pattern, line):
#             print(line)
            # add: no search in comments.
            b_lnr       = lnr
            if not only_begining:
                _open       = len(re.findall("({)", line))
                _close      = len(re.findall("(})", line))
                while _open != _close:
                    lnr+=1
                    line     = file_l[lnr-1]
                    _open   += len(re.findall("({)", line))
                    _close  += len(re.findall("(})", line))
                e_lnr        = lnr
                defi_dict[file].append([ b_lnr, e_lnr ])
            else:
                defi_dict[file].append([ b_lnr, b_lnr ])
            lnr         += 1
        else:
            lnr+=1
vim.command("let s:defi_dict_py="+str(defi_dict))
vim.command("let g:defi_dict_py="+str(defi_dict))
ENDPYTHON
return s:defi_dict_py
endfunction
"}}}

" Find all names of locally defined commands, colors and environments. 
" Used by the completion function.
"{{{ LocalCommands_vim 
" a:1 = pattern
" a:2 = "!" => renegenerate the input files.
function! <SID>LocalCommands_vim(...)
"     let time = reltime()
    let pattern = a:0 >= 1 && a:1 != '' ? a:1 : '\\def\>\|\\newcommand\>\|\\newenvironment\|\\newtheorem\|\\definecolor\|'
		\ . '\\Declare\%(RobustCommand\|FixedFont\|TextFontCommand\|MathVersion\|SymbolFontAlphabet'
			    \ . '\|MathSymbol\|MathDelimiter\|MathAccent\|MathRadical\|MathOperator\)'
		\ . '\|\\SetMathAlphabet\>'
    let bang	= a:0 >= 2 ? a:2 : '' 

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)

    " Makeing lists of commands and environments found in input files
    if bang == "!" || !exists("b:TreeOfFiles")
	 " Update the cached values:
	 let [ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ] = TreeOfFiles(atp_MainFile)
     endif
     let [ Tree, List, Type_Dict, Level_Dict ] = deepcopy([ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ])

     let saved_loclist	= getloclist(0)
     " I should scan the preambule separately!
     " This will make the function twice as fast!
     silent! execute "lvimgrep /".pattern."/j " . fnameescape(atp_MainFile)
     for file in List
	 if get(Type_Dict, file, 'no_file') == 'preambule'
	     silent! execute "lvimgrepadd /".pattern."/j " . fnameescape(file)
	 endif
     endfor
     let loclist	= getloclist(0)
     call setloclist(0, saved_loclist)

     let atp_LocalCommands	= []
     let atp_LocalEnvironments	= []
     let atp_LocalColors	= []

     for line in loclist
	" the order of pattern is important
	if line['text'] =~ '^[^%]*\\definecolor'
	    " color name
	    let name=matchstr(line['text'],
			\ '\\definecolor\s*{\s*\zs[^}]*\ze\s*}')
	    let type="Colors"
	elseif line['text'] =~ '^[^%]*\%(\\def\>\|\\newcommand\)'
	    " definition name 
	    let name= '\' . matchstr(line['text'], '\\def\\\zs[^{]*\ze{\|\\newcommand{\?\\\zs[^\[{]*\ze}')
	    let name=substitute(name, '\(#\d\+\)\+\s*$', '{', '')
	    if name =~ '#\d\+'
		echo line['text']
		echo name
	    endif
	    let type="Commands"
	    " definition
" 	    let def=matchstr(line['text'],
" 			\ '^\%(\\def\\[^{]*{\zs.*\ze}\|\\newcommand\\[^{]*{\zs.*\ze}\)') 
	elseif line['text'] =~ '^[^%]*\%(\\Declare\%(RobustCommand\|FixedFont\|TextFontCommand\|MathVersion\|SymbolFontAlphabet'
			    \ . '\|MathSymbol\|MathDelimiter\|MathAccent\|MathRadical\|MathOperator\)\>\|\\SetMathAlphabet\)'
	    let name=matchstr(line['text'],
			\ '\%(\\Declare\%(RobustCommand\|FixedFont\|TextFontCommand\|MathVersion\|SymbolFontAlphabet'
			    \ . '\|MathSymbol\|MathDelimiter\|MathAccent\|MathRadical\|MathOperator\)\|\\SetMathAlphabet\)\s*{\s*\zs[^}]*\ze\s*}')
	    let type="Commands"
	elseif line['text'] =~ '^[^%]*\%(\\newenvironment\|\\newtheorem\)'
	    " environment name
	    let name=matchstr(line['text'],
			\ '^[^%]*\\\%(newtheorem\*\?\|newenvironment\)\s*{\s*\zs[^}]*\ze\s*}')
	    let type="Environments"
	endif
	if exists("name") && name != '' && name != '\'
	    if count(atp_Local{type}, name) == 0
		call add(atp_Local{type}, name)
	    endif
	endif
    endfor

    let b:atp_LocalCommands		= atp_LocalCommands
    let b:atp_LocalEnvironments		= atp_LocalEnvironments
    let b:atp_LocalColors		= atp_LocalColors
    return [ atp_LocalEnvironments, atp_LocalCommands, atp_LocalColors ]

endfunction "}}}
" {{{ LocalCommands_py
function! <SID>LocalCommands_py(write, ...)
    " The first argument pattern is not implemented
    " but it should be a python regular expression
    let bang	= a:0 >= 2 ? a:2 : '' 

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)

    if a:write
	call atplib#write()
    endif


    " Makeing lists of commands and environments found in input files
    if bang == "!" || !exists("b:TreeOfFiles")
	 " Update the cached values:
	 let [ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ] = TreeOfFiles(atp_MainFile)
     endif
     let [ Tree, List, Type_Dict, Level_Dict ] = deepcopy([ b:TreeOfFiles, b:ListOfFiles, b:TypeDict, b:LevelDict ])

     " Note: THIS CODE IS ONLY NEEDED WHEN PWD is different than the TreeOfFiles was
     " called! There is an option to store full path in ATP, then this is not needed.
     let files = []
     for file in b:ListOfFiles
	 if b:TypeDict[file] == "input" || b:TypeDict[file] == "preambule" 
	     if filereadable(file)
		 call add(files, file)
	     else
		 let file=atplib#KpsewhichFindFile("tex", file)
		 if file != ""
		     call add(files, file)
		 endif
	     endif
	 endif
     endfor
python << END
import re, vim

pattern=re.compile('\s*(?:\\\\(?P<def>def)(?P<def_c>\\\\[^#{]*)|(?:\\\\(?P<nc>(?:re)?newcommand)|\\\\(?P<env>(?:re)?newenvironment)|\\\\(?P<nt>(?:re)?newtheorem\*?)|\\\\(?P<col>definecolor)|\\\\(?P<dec>Declare)(?:RobustCommand|FixedFont|TextFontCommand|MathVersion|SymbolFontAlphabet|MathSymbol|MathDelimiter|MathAccent|MathRadical|MathOperator)\s*{|\\\\(?P<sma>SetMathAlphabet))\s*{(?P<arg>[^}]*)})')

files=[vim.eval("atp_MainFile")]+vim.eval("files")
localcommands   =[]
localcolors     =[]
localenvs       =[]
for file in files:
    lnr=1
    try:
        file_ob=open(file, 'r')
        file_l=file_ob.read().split("\n")
        file_ob.close()
        for line in file_l:
            m=re.match(pattern, line)
            if m:
                if m.group('def'):
                    localcommands.append(m.group('def_c'))
                elif m.group('nc') or m.group('dec') or m.group('sma'):
                    localcommands.append(m.group('arg'))
                elif m.group('nt') or m.group('env'):
                    localenvs.append(m.group('arg'))
                elif m.group('col'):
                    localcolors.append(m.group('arg'))
    except IOError:
        pass
vim.command("let atp_LocalCommands="+str(localcommands))
vim.command("let atp_LocalEnvironments="+str(localenvs))
vim.command("let atp_LocalColors="+str(localcolors))
END
if exists("atp_LocalCommands")
    let b:atp_LocalCommands=map(atp_LocalCommands, 'substitute(v:val, ''\\\\'', ''\'', '''')')
else
    let b:atp_LocalCommands=[]
endif
if exists("b:atp_LocalColors")
    let b:atp_LocalColors=map(atp_LocalColors, 'substitute(v:val, ''\\\\'', ''\'', '''')')
else
    let b:atp_LocalColors=[]
endif
if exists("b:atp_LocalEnvironments")
    let b:atp_LocalEnvironments=map(atp_LocalEnvironments, 'substitute(v:val, ''\\\\'', ''\'', '''')')
else
    let b:atp_LocalEnvironments=[]
endif
return [ b:atp_LocalEnvironments, b:atp_LocalCommands, b:atp_LocalColors ]
endfunction
"}}}
" {{{ LocalCommands
function! LocalCommands(write, ...)
    let time=reltime()
    let pattern = a:0 >= 1 && a:1 != '' ? a:1 : '\\def\>\|\\newcommand\>\|\\newenvironment\|\\newtheorem\|\\definecolor\|'
		\ . '\\Declare\%(RobustCommand\|FixedFont\|TextFontCommand\|MathVersion\|SymbolFontAlphabet'
			    \ . '\|MathSymbol\|MathDelimiter\|MathAccent\|MathRadical\|MathOperator\)'
		\ . '\|\\SetMathAlphabet\>'
    let bang	= a:0 >= 2 ? a:2 : '' 

    if has("python")
	call <SID>LocalCommands_py(a:write, '' , bang)
    else
	call <SID>LocalCommands_vim(pattern, bang)
    endif
    let g:time_LocalCommands=reltimestr(reltime(time))
endfunction
" }}}

" Search for Definition in the definition dictionary (s:make_defi_dict).
"{{{ Dsearch
function! <SID>Dsearch(bang,...)

    call atplib#write()

    let time		= reltime()
    let o_pattern	= a:0 >= 1 ? a:1 : ''
    let pattern		= '\%(\\def\|\\\%(re\)\=newcommand\s*{\=\|\\providecommand\s*{\=\|\\\%(re\)\=newenvironment\s*{\|\\\%(re\)\=newtheorem\s*{\|\\definecolor\s*{\)\s*\\\=\w*\zs'.o_pattern
    let preambule_only	= ( a:bang == "!" ? 0 : 1 )
    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)

    if has("python")
	let defi_dict	= s:make_defi_dict_py(a:bang, atp_MainFile, pattern)
    else
	let defi_dict	= s:make_defi_dict_vim(a:bang, atp_MainFile, pattern)
    endif

    if len(defi_dict) > 0
	" wipe out the old buffer and open new one instead
	if bufloaded("DefiSearch")
	    exe "silent bd! " . bufnr("DefiSearch") 
	endif
	let b:atp_MainFile	= expand("%")
	let b:atp_ProjectDir	= expand("%:p:h")
	setl syntax=tex

	let defi_list = []
" 	let g:defi_list = defi_list

	for inputfile in keys(defi_dict)
	    let ifile	= readfile(inputfile)
	    for l:range in defi_dict[inputfile]
		" This respects the options 'smartcase' and 'ignorecase'.
		let case = ( &l:smartcase && &l:ignorecase && pattern =~ '\u' ? 'noignorecase'  : ( &l:ignorecase ? 'ignorecase' : 'noignorecase' )) 
		let condition = ( case == "noignorecase" ? ifile[l:range[0]-1] =~# pattern : ifile[l:range[0]-1] =~? pattern )
		if condition
		    " print the lines into defi_list
		    let i=0
		    let c=0
		    " add an empty line if the definition is longer than one line
		    if l:range[0] != l:range[1]
			call add(defi_list, '')
" 			call setline(line('$')+1,'')
			let i+=1
		    endif
		    while c <= l:range[1]-l:range[0] 
			let line=l:range[0]+c
			call add(defi_list, ifile[line-1])
" 			call setline(line('$')+1,ifile[line-1])
			let i+=1
			let c+=1
		    endwhile
		endif
	    endfor
	endfor

	if len(defi_list) == 0
	    redraw
	    echohl ErrorMsg
	    if a:bang == "!"
		echomsg "[ATP:] definition not found."
	    else
		echomsg "[ATP:] definition not found in the preambule, try with a bang ! to search beyond."
	    endif
	    echohl Normal
	    return
	endif

	let window_height= min([g:atp_DsearchMaxWindowHeight, len(defi_list)])
	" open new buffer
	let openbuffer=" +setl\\ buftype=nofile\\ nospell " . fnameescape("DefiSearch")
	if g:vertical == 1
	    let openbuffer="keepalt vsplit " . openbuffer 
	else
	    let openbuffer="keepalt rightbelow ".window_height."split " . openbuffer 
	endif

	silent exe openbuffer
	call setline(1, defi_list)
	if o_pattern != ""
	    call matchadd('Search', ( &l:ignorecase ? '\c' : '\C' ) .o_pattern)
	    let @/=o_pattern
	endif
	setl syntax=tex
	setl readonly
	map <buffer> <silent> q	:bd<CR>
    else
	redraw
	echohl ErrorMsg
	if a:bang == "!"
	    echomsg "[ATP:] definition not found."
	else
	    echomsg "[ATP:] definition not found in the preambule, try with a bang ! to search beyond."
	endif
	echohl Normal
    endif
    let g:source_time_DSEARCH=reltimestr(reltime(time))
endfunction
function! DsearchComp(ArgLead, CmdLine, CursorPos)
    if !exists("b:atp_LocalCommands")
        LocalCommands
    endif
    let list=[]
    call extend(list, b:atp_LocalCommands)
    call extend(list, b:atp_LocalColors)
    call extend(list, b:atp_LocalEnvironments)
    call filter(list, 'v:val =~ a:ArgLead')
    call map(list, 'escape(v:val, ''\*'')')
    return sort(list)
endfunction
"}}}

" Search in tree and return the one level up element and its line number.
" {{{ <SID>SearchInTree
" Before running this function one has to set the two variables
" s:branch/s:branch_line to 0.
" the a:tree variable should be something like:
" a:tree = { b:atp_MainFile, [ TreeOfFiles(b:atp_MainFile)[0], 0 ] }
" necessary a rooted tree!

" This function remaps keys of dictionary.
function! MapDict(dict) 
    let new_dict = {}
    for key in keys(a:dict)
	let new_key = fnamemodify(key, ":p")
	let new_dict[new_key] = a:dict[key] 
    endfor
    return new_dict
endfunction

function! <SID>SearchInTree(tree, branch, what)
    if g:atp_debugSIT
	exe "redir! > ".g:atp_TempDir."/SearchInTree.log"
	silent! echo "___SEARCH_IN_TREE___"
	silent! echo "a:branch=". a:branch
	silent! echo "a:what=" . a:what
    endif
    if g:atp_debugSIT >= 2
	silent! echo "a:tree=" . string(a:tree)
    endif
	
"     let branch	= a:tree[a:branch][0]
    if a:branch =~ '^\s*\/'
	let cwd		= getcwd()
	exe "lcd " . fnameescape(b:atp_ProjectDir)
	let branchArg	= ( g:atp_RelativePath 	? fnamemodify(a:branch, ":.") 	: a:branch  )
	let branchArgN	= ( !g:atp_RelativePath ? fnamemodify(a:branch, ":.") 	: a:branch  )
	let whatArg	= ( g:atp_RelativePath 	? fnamemodify(a:what, ":.") 	: a:what  )
	let whatArgN	= ( !g:atp_RelativePath ? fnamemodify(a:what, ":.") 	: a:what  )
	if g:atp_debugSIT
	    silent! echo "*** cwd=" . getcwd() . " b:atp_ProjectDir= " . b:atp_ProjectDir . " " . fnamemodify(a:branch, ":.") . " " . a:branch
	endif
	exe "lcd " . fnameescape(cwd)
    else
	let branchArg	= ( g:atp_RelativePath 	? a:branch 	: atplib#FullPath(a:branch) )
	let branchArgN	= ( !g:atp_RelativePath ? a:branch 	: atplib#FullPath(a:branch) )
	let whatArg	= ( g:atp_RelativePath 	? a:what 	: atplib#FullPath(a:what) )
	let whatArgN	= ( !g:atp_RelativePath ? a:what 	: atplib#FullPath(a:what) )
    endif
    if g:atp_debugSIT
	silent! echo "branchArg=" . branchArg . " branchArgN=" . branchArgN
	silent! echo "whatArg=" . whatArg . " whatArgN=" . whatArgN
    endif
    let branch	= get(a:tree, branchArg , get(a:tree, branchArgN, ['NO_BRANCH']))[0]
    if count(keys(branch), whatArg) || count(keys(branch), whatArgN)
	" The following variable is used as a return value in
	" RecursiveSearch!
	let g:ATP_branch	= branchArg
	let g:ATP_branch_line	= get(branch, whatArg, get(branch, whatArgN, ['', 'ERROR']))[1]
	if g:atp_debugSIT
	    silent! echo "g:ATP_branch=" . g:ATP_branch . "   g:ATP_branch_line=" . g:ATP_branch_line
	    redir END
	endif
	return branchArg
    else
	for new_branch in keys(branch)
	    call <SID>SearchInTree(branch, new_branch, whatArg)
	endfor
    endif
    if g:atp_debugSIT
	redir END
    endif
    return
endfunction
" }}}

" Search in all input files recursively.
" {{{ Search (recursive)
"
" Variables are used to pass them to next runs (this function calls it self) 
" a:main_file	= b:atp_MainFile
" a:start_file	= expand("%:p") 	/this variable will not change untill the
" 						last instance/ 
" a:tree	= make_tree 		=> make a tree
" 		= any other value	=> use { a:main_file : [ b:TreeOfFiles, 0] }	
" a:cur_branch	= expand("%") 		/this will change whenever we change a file/
" a:call_nr	= number of the call			
" a:wrap_nr	= if hit top/bottom a:call=0 but a:wrap_nr+=1
" a:winsaveview = winsaveview(0)  	to resotre the view if the pattern was not found
" a:bufnr	= bufnr("%")		to come back to begining buffer if pattern not found
" a:strftime	= strftime(0)		to compute the time
" a:pattern	= 			pattern to search
" a:1		=			flags: 'bcewWs'
" a:2 is not not used:
" a:2		= 			goto = DOWN_ACCEPT / Must not be used by the end user/
" 					0/1 1=DOWN_ACCEPT	
" 								
" g:atp_debugRS 	if 1 sets debugging messages which are appended to '/tmp/ATP_rs_debug' 
			" you can :set errorfile=/tmp/ATP_rs_debug
			" and	  :set efm=.*
			" if 2 show time
" log file : /tmp/ATP_rs_debug
" {{{ s:RecursiveSearch function
let g:atp_swapexists = 0
try
function! <SID>RecursiveSearch(main_file, start_file, maketree, tree, cur_branch, call_nr, wrap_nr, winsaveview, bufnr, strftime, vim_options, cwd, pattern, ... )

    let main_file	= g:atp_RelativePath ? atplib#RelativePath(a:main_file, b:atp_ProjectDir) : a:main_file
	
    let time0	= reltime()

    " set and restore some options:
    " foldenable	(unset to prevent opening the folds :h winsaveview)
    " comeback to the starting buffer
    if a:call_nr == 1 && a:wrap_nr == 1

	if a:vim_options	== { 'no_options' : 'no_options' }
	    let vim_options 	=  { 'hidden'	: &l:hidden, 
				\ 'foldenable' 	: &l:foldenable,
				\ 'autochdir'	: &l:autochdir }
	else
	    let vim_options	= a:vim_options
	endif
	let &l:hidden		= 1
	let &l:foldenable	= 0
	let &l:autochdir	= 0

	if a:cwd		== 'no_cwd'
	    let cwd		=  getcwd()
	else
	    let cwd		= a:cwd
	endif
	exe "lcd " . fnameescape(b:atp_ProjectDir)

	" This makes it work faster when the input files were not yet opened by vim 
	" some of them will not be shown to the user.
" 	syntax off
	filetype off 
	" there are many errors in /tmp/ATP_rs_debug file due to this which are not
	" important.

    else
	let vim_options		= a:vim_options
	let cwd			= a:cwd
    endif

	    " Redirect debuggin messages:
	    if g:atp_debugRS
		if a:wrap_nr == 1 && a:call_nr == 1
		    exe "redir! > ".g:atp_TempDir."/RecursiveSearch.log"
		else
		    exe "redir! >> ".g:atp_TempDir."/RecursiveSearch.log"
		endif
		silent echo "________________"
		silent echo "Args: a:pattern:".a:pattern." call_nr:".a:call_nr. " wrap_nr:".a:wrap_nr . " cwd=" . getcwd()
	    endif

    	let flags_supplied = a:0 >= 1 ? a:1 : ""

	if flags_supplied =~# 'p'
	    let flags_supplied = substitute(flags_supplied, 'p', '', 'g')
	    echohl WarningMsg
	    echomsg "[ATP:] searching flag 'p' is not supported, filtering it out."
	    echohl Normal
	endif

	if a:maketree == 'make_tree'
	    if g:atp_debugRS
	    silent echo "*** Makeing Tree ***"
	    endif
	    let tree_of_files 	= TreeOfFiles(main_file)[0]
	else
	    if g:atp_debugRS
	    silent echo "*** Using Tree ***"
	    endif
	    let tree_of_files	= a:tree
	endif
	let tree	= { main_file : [ tree_of_files, 0 ] }

	if a:cur_branch != "no cur_branch "
	    let cur_branch	= a:cur_branch
	else
	    let cur_branch	= main_file
	endif

		if g:atp_debugRS > 1
		    silent echo "TIME0:" . reltimestr(reltime(time0))
		endif

	let pattern		= a:pattern
	let flags_supplied	= substitute(flags_supplied, '[^bcenswWS]', '', 'g')

    	" Add pattern to the search history
	if a:call_nr == 1
	    call histadd("search", a:pattern)
	    let @/ = a:pattern
	endif

	" Set up searching flags
	let flag	= flags_supplied
	if a:call_nr > 1 
	    let flag	= flags_supplied !~# 'c' ? flags_supplied . 'c' : flags_supplied
	endif
	let flag	= substitute(flag, 'w', '', 'g') . 'W'
	let flag	= flag !~# 'n' ? substitute(flag, 'n', '', 'g') . 'n' : flag
	let flag	= substitute(flag, 's', '', 'g')

	if flags_supplied !~# 'b'
	    " forward searching flag for input files:
	    let flag_i	= flags_supplied !~# 'c' ? flags_supplied . 'c' : flags_supplied
	else
	    let flag_i	= substitute(flags_supplied, 'c', '', 'g')
	endif
	let flag_i	= flag_i !~# 'n' ? flag_i . 'n' : flag_i
	let flag_i	= substitute(flag_i, 'w', '', 'g') . 'W'
	let flag_i	= substitute(flag_i, 's', '', 'g')

		if g:atp_debugRS
		silent echo "      flags_supplied:".flags_supplied." flag:".flag." flag_i:".flag_i." a:1=".(a:0 != 0 ? a:1 : "")
		endif

	" FIND PATTERN: 
	let cur_pos		= [line("."), col(".")]
	" We filter out the 's' flag which should be used only once
	" as the flags passed to next <SID>RecursiveSearch()es are based on flags_supplied variable
	" this will work.
	let s_flag		= flags_supplied =~# 's' ? 1 : 0
	let flags_supplied	= substitute(flags_supplied, 's', '', 'g')
	if s_flag
	    call setpos("''", getpos("."))
	endif
	keepjumps let pat_pos	= searchpos(pattern, flag)

		if g:atp_debugRS > 1
		    silent echo "TIME1:" . reltimestr(reltime(time0))
		endif

	" FIND INPUT PATTERN: 
	" (we do not need to search further than pat_pos)
	if pat_pos == [0, 0]
	    let stop_line	= flag !~# 'b' ? line("$")  : 1
	else
	    let stop_line	= pat_pos[0]
	endif
	keepjumps let input_pos	= searchpos('\m^[^%]*\\input\s*{', flag_i . 'n', stop_line )

		if g:atp_debugRS > 1
		    silent echo "TIME2:" . reltimestr(reltime(time0))
		endif

		if g:atp_debugRS
		silent echo "Positions: ".string(cur_pos)." ".string(pat_pos)." ".string(input_pos)." in branch: ".cur_branch."#".expand("%:p") . " stop_line: " . stop_line 
		endif

	" Down Accept:
	" the current value of down_accept
	let DOWN_ACCEPT = a:0 >= 2 ? a:2 : 0
	" the value of down_accept in next turn 
	let down_accept	= getline(input_pos[0]) =~ pattern || input_pos == [0, 0] ?  1 : 0

" 		if g:atp_debugRS
" 		    silent echo "DOWN_ACCEPT=" . DOWN_ACCEPT . " down_accept=" . down_accept
" 		endif

	" Decide what to do: accept the pattern, go to higher branch, go to lower
	" branch or say Pattern not found
	if flags_supplied !~# 'b'
	    " FORWARD
	    " cur < pat <= input
	    if atplib#CompareCoordinates(cur_pos,pat_pos) && atplib#CompareCoordinates_leq(pat_pos, input_pos)
		let goto_s	= 'ACCEPT'
	    " cur == pat <= input
	    elseif cur_pos == pat_pos && atplib#CompareCoordinates_leq(pat_pos, input_pos)
		" this means that the 'flag' variable has to contain 'c' or the
		" wrapscan is on
		" ACCEPT if 'c' and wrapscan is off or there is another match below,
		" if there is not go UP.
		let wrapscan	= ( flags_supplied =~# 'w' || &l:wrapscan && flags_supplied !~# 'W' )
		if flag =~# 'c'
		let goto_s	= 'ACCEPT'
		elseif wrapscan
		    " if in wrapscan and without 'c' flag
		let goto_s	= 'UP'
		else
		    " this should not happen: cur == put can hold only in two cases:
		    " wrapscan is on or 'c' is used.
		    let goto_s	= 'ERROR'
		endif
	    " pat < cur <= input
	    elseif atplib#CompareCoordinates(pat_pos, cur_pos) && atplib#CompareCoordinates_leq(cur_pos, input_pos) 
		let goto_s	= 'UP'
	    " cur < input < pat
	    elseif atplib#CompareCoordinates(cur_pos, input_pos) && atplib#CompareCoordinates(input_pos, pat_pos)
		let goto_s	= 'UP'
	    " cur < input == pat 		/we are looking for '\\input'/
	    elseif atplib#CompareCoordinates(cur_pos, input_pos) && input_pos == pat_pos
		let goto_s	= 'ACCEPT'
	    " input < cur <= pat	(includes input = 0])
	    elseif atplib#CompareCoordinates(input_pos, cur_pos) && atplib#CompareCoordinates_leq(cur_pos, pat_pos)
		" cur == pat thus 'flag' contains 'c'.
		let goto_s	= 'ACCEPT'
	    " cur == input
	    elseif cur_pos == input_pos
		let goto_s	= 'UP'
	    " cur < input < pat
	    " input == 0 			/there is no 'input' ahead - flag_i contains 'W'/
	    " 					/but there is no 'pattern ahead as well/
	    " at this stage: pat < cur 	(if not then  input = 0 < cur <= pat was done above).
	    elseif input_pos == [0, 0]
		if expand("%:p") == fnamemodify(main_file, ":p")
		    " wrapscan
		    if ( flags_supplied =~# 'w' || &l:wrapscan  && flags_supplied !~# 'W' )
			let new_flags	= substitute(flags_supplied, 'w', '', 'g') . 'W'  
			if a:wrap_nr <= 2
			    call cursor(1,1)

				if g:atp_debugRS
				silent echo " END 1 new_flags:" . new_flags 
				redir END
				endif

			    keepjumps call <SID>RecursiveSearch(main_file, a:start_file, "", tree_of_files, a:cur_branch, 1, a:wrap_nr+1, a:winsaveview, a:bufnr, a:strftime, vim_options, cwd, pattern, new_flags) 

			    return
			else
			    let goto_s 	= "REJECT"
" 			    echohl ErrorMsg
" 			    echomsg 'Pattern not found: ' . a:pattern
" 			    echohl None
			endif
		    else
			let goto_s 	= "REJECT"
" 			echohl ErrorMsg
" 			echomsg 'Pattern not found: ' . a:pattern
" 			echohl None
		    endif
		" if we are not in the main file go up.
		else
		    let goto_s	= "DOWN"
		endif
	    else
		let goto_s 	= 'ERROR'
	    endif
	else
	    " BACKWARD
	    " input <= pat < cur (pat != 0)
	    if atplib#CompareCoordinates(pat_pos, cur_pos) && atplib#CompareCoordinates_leq(input_pos, pat_pos) && pat_pos != [0, 0]
		" input < pat
		if input_pos != pat_pos
		    let goto_s	= 'ACCEPT'
		" input == pat
		else
		    let goto_s	= 'UP'
		endif
	    " input <= pat == cur (input != 0)			/pat == cur => pat != 0/
	    elseif cur_pos == pat_pos && atplib#CompareCoordinates_leq(input_pos, pat_pos) && input_pos != [0, 0]
		" this means that the 'flag' variable has to contain 'c' or the
		" wrapscan is on
		let wrapscan	= ( flags_supplied =~# 'w' || &l:wrapscan  && flags_supplied !~# 'W' )
		if flag =~# 'c'
		    let goto_s 	= 'ACCEPT'
		elseif wrapscan
		    " if in wrapscan and without 'c' flag
		    let goto_s	= 'UP'
		else
		    " this should not happen: cur == put can hold only in two cases:
		    " wrapscan is on or 'c' is used.
		    let goto_s	= 'ERROR'
		endif
	    " input <= cur < pat (input != 0)
	    elseif atplib#CompareCoordinates(cur_pos, pat_pos) && atplib#CompareCoordinates_leq(input_pos, cur_pos) && input_pos != [0, 0] 
		let goto_s	= 'UP'
	    " pat < input <= cur (input != 0)
	    elseif atplib#CompareCoordinates_leq(input_pos, cur_pos) && atplib#CompareCoordinates(pat_pos, input_pos) && input_pos != [0, 0]
		let goto_s	= 'UP'
	    " input == pat < cur (pat != 0) 		/we are looking for '\\input'/
	    elseif atplib#CompareCoordinates(input_pos, cur_pos) && input_pos == pat_pos && pat_pos != [0, 0]
		let goto_s	= 'ACCEPT'
	    " pat <= cur < input (pat != 0) 
	    elseif atplib#CompareCoordinates(cur_pos, input_pos) && atplib#CompareCoordinates_leq(pat_pos, cur_pos) && input_pos != [0, 0]
		" cur == pat thus 'flag' contains 'c'.
		let goto_s	= 'ACCEPT'
	    " cur == input
	    elseif cur_pos == input_pos
		let goto_s 	= 'UP'
	    " input == 0 			/there is no 'input' ahead - flag_i contains 'W'/
	    " 					/but there is no 'pattern ahead as well/
	    " at this stage: cur < pat || pat=input=0  (if not then  pat <= cur was done above, input=pat=0 is the 
	    " 						only posibility to be passed by the above filter).
	    elseif input_pos == [0, 0]
		" I claim that then cur < pat or pat=0
		if expand("%:p") == fnamemodify(main_file, ":p")
		    " wrapscan
		    if ( flags_supplied =~# 'w' || &l:wrapscan  && flags_supplied !~# 'W' )
			let new_flags	= substitute(flags_supplied, 'w', '', 'g') . 'W'  
			if a:wrap_nr <= 2
			    call cursor(line("$"), col("$"))

				if g:atp_debugRS
				silent echo " END 2 new_flags:".new_flags
				redir END
				endif

			    keepjumps call <SID>RecursiveSearch(main_file, a:start_file, "", tree_of_files, a:cur_branch, 1, a:wrap_nr+1, a:winsaveview, a:bufnr, a:strftime, vim_options, cwd, pattern, new_flags) 

				if g:atp_debugRS > 1
				    silent echo "TIME_END:" . reltimestr(reltime(time0))
				endif

			    return
			else
			    let goto_s 	= "REJECT"
" 			    echohl ErrorMsg
" 			    echomsg 'Pattern not found: ' . a:pattern
" 			    echohl None
			endif
		    else
			let goto_s 	= "REJECT"
		    endif
		" if we are not in the main file go up.
		else
		    let goto_s	= "DOWN" 
		    " If using the following line DOWN_ACCEPT and down_accept
		    " variables are not needed. This seems to be the best way.
		    " 	There is no need to use this feature for
		    " 	\input <file_name> 	files.
		    if pattern =~ '\\\\input' || pattern =~ '\\\\include'
" 			if getline(input_pos[0]) =~ pattern || getline(".") =~ pattern
			let goto_s	= "DOWN_ACCEPT"
		    endif
		endif
	    else
		let goto_s 	= 'ERROR'
	    endif
	endif

		if g:atp_debugRS
		silent echo "goto_s:".goto_s
		endif
		if g:atp_debugRS >= 2
		    silent echo "TIME ***goto*** " . reltimestr(reltime(time0))
		endif

	" When ACCEPTING the line:
	if goto_s == 'ACCEPT'
	    keepjumps call setpos(".", [ 0, pat_pos[0], pat_pos[1], 0])
	    if flags_supplied =~#  'e'
		keepjumps call search(pattern, 'e', line("."))
	    endif
	    "A Better solution must be found.
" 	    if &l:hlsearch
" 		execute '2match Search /'.pattern.'/'
" 	    endif
		
	    let time	= matchstr(reltimestr(reltime(a:strftime)), '\d\+\.\d\d\d') . "sec."

	    if a:wrap_nr == 2 && flags_supplied =~# 'b'
		redraw
		echohl WarningMsg
		echo "search hit TOP, continuing at BOTTOM "
		echohl Normal
	    elseif a:wrap_nr == 2
		redraw
		echohl WarningMsg
		echo "search hit BOTTOM, continuing at TOP "
		echohl Normal
	    endif


		if g:atp_debugRS
		silent echo "FOUND PATTERN: " . a:pattern . " time " . time
		silent echo ""
		redir END
		endif

		" restore vim options 
		if a:vim_options != { 'no_options' : 'no_options' }
		    for option in keys(a:vim_options)
			execute "let &l:".option."=".a:vim_options[option]
		    endfor
		endif
		exe "lcd " . fnameescape(cwd)
" 		syntax enable
		filetype on
		filetype detect

	    return

	" when going UP
	elseif goto_s == 'UP'
	    call setpos(".", [ 0, input_pos[0], input_pos[0], 0])
	    " Open file and Search in it"
	    " This should be done by kpsewhich:
	    let file = matchstr(getline(input_pos[0]), '\\input\s*{\zs[^}]*\ze}')
	    let file = atplib#append_ext(file, '.tex')

	    let open =  flags_supplied =~ 'b' ? 'keepalt edit + ' : 'keepalt edit +1 '
	    let swapfile = globpath(fnamemodify(file, ":h"), ( has("unix") ? "." : "" ) . fnamemodify(file, ":t") . ".swp")

	    if !( a:call_nr == 1 && a:wrap_nr == 1 )
		let open = "silent keepjumps " . open
	    endif
 
	    let projectVarDict 	= SaveProjectVariables()
" 	    let projectScript	= SaveProjectVariables("g:atp_ProjectLocalVariables") 
" 	    let atp_ProjectScript 	= [ exists("g:atp_ProjectScript") ? g:atp_ProjectScript : b:atp_ProjectScript, exists("g:atp_ProjectScript") ] 
" 	    let g:atp_ProjectScript 	= 0
	    if g:atp_debugRS >= 3
		silent echo "projectVarDict : " . string(projectVarDict) 
		let g:projectVarDict = projectVarDict
	    elseif g:atp_debugRS >= 2
		let g:projectVarDict = projectVarDict
	    endif
	    if g:atp_debugRS >= 2
		silent echo "TIME ***goto UP before open*** " . reltimestr(reltime(time0))
	    endif
	    " OPEN:
	    if empty(swapfile) || bufexists(file)
		if g:atp_debugRS >= 2
		silent echo "Alternate (before open) " . bufname("#")
		endif
		silent! execute open . fnameescape(file)
	    else
		echoerr "Recursive Search: swap file: " . swapfile . " exists. Aborting." 
		return
	    endif
	    if g:atp_debugRS >= 2
		exe "redir! >> ".g:atp_TempDir."/RecursiveSearch.log"
		silent echo "TIME ***goto UP after open*** " . reltimestr(reltime(time0))
	    endif
" 	    call RestoreProjectVariables(projectScript)
" 	    if atp_ProjectScript[1]
" 		let g:atp_ProjectScript = atp_ProjectScript[0]
" 	    else
" 		unlet g:atp_ProjectScript
" 	    endif
	    call RestoreProjectVariables(projectVarDict)
	    if g:atp_debugRS >= 2
		silent echo "TIME ***goto UP restore variables *** " . reltimestr(reltime(time0))
	    endif

	    if flags_supplied =~# 'b'
		call cursor(line("$"), col("$"))
	    else
		call cursor(1,1)
	    endif

		if g:atp_debugRS
		silent echo "In higher branch:      " . file . " POS " line(".").":".col(".") 
		silent echo "Open Command:          '" . open . file . "'"
		silent echo "exist b:TreeOfFiles    " . exists("b:TreeOfFiles")
		silent echo "flags_supplied:        " . flags_supplied
		endif
		if g:atp_debugRS >= 2
		silent echo "Alternate (after open) " . bufname("#")
		endif

		if g:atp_debugRS >= 2
		silent echo "TIME_END:              " . reltimestr(reltime(time0))
		endif

" 	    let flag	= flags_supplied =~ 'W' ? flags_supplied : flags_supplied . 'W'
	    keepalt keepjumps call <SID>RecursiveSearch(main_file, a:start_file, "", tree_of_files, expand("%:p"), a:call_nr+1, a:wrap_nr, a:winsaveview, a:bufnr, a:strftime, vim_options, cwd, pattern, flags_supplied, down_accept)

	    if g:atp_debugRS
		redir END
	    endif
	    return

	" when going DOWN
	elseif goto_s == 'DOWN' || goto_s == 'DOWN_ACCEPT'
	    " We have to get the element in the tree one level up + line number
	    let g:ATP_branch 		= "nobranch"
	    let g:ATP_branch_line	= "nobranch_line"

		if g:atp_debugRS
		silent echo "     SearchInTree Args " . expand("%:p")
		endif

	    if g:atp_RelativePath
		call <SID>SearchInTree(l:tree, main_file, atplib#RelativePath(expand("%:p"), resolve(b:atp_ProjectDir)))
	    else
		call <SID>SearchInTree(l:tree, main_file, expand("%:p"))
	    endif

		if g:atp_debugRS
		silent echo "     SearchInTree found " . g:ATP_branch . " g:ATP_branch_line=" . g:ATP_branch_line
		endif

	    if g:ATP_branch == "nobranch"
		echohl ErrorMsg
		echomsg "[ATP:] this probably happend while searching for \\input, it is not yet supported, if not it is a bug"
		echohl Normal

		silent! echomsg "Tree=" . string(l:tree)
		silent! echomsg "MainFile " . main_file . " current_file=" . expand("%:p")
		silent! echomsg "Going to file : " . g:ATP_branch . " ( g:ATP_branch ) "

	    	" restore the window and buffer!
		silent execute "keepjumps keepalt edit #" . a:bufnr
		call winrestview(a:winsaveview)

		return
	    endif

	    let next_branch = atplib#FullPath(g:ATP_branch)
	    let swapfile = globpath(fnamemodify(next_branch, ":h"), ( has("unix") ? "." : "" ) . fnamemodify(next_branch, ":t") . ".swp")
	    if a:call_nr == 1 && a:wrap_nr == 1 
		let open =  'silent keepalt edit +'.g:ATP_branch_line." ".fnameescape(next_branch)
	    else
		let open =  'silent keepjumps keepalt edit +'.g:ATP_branch_line." ".fnameescape(next_branch)
	    endif

	    if g:atp_debugRS >= 2
		silent echo "TIME ***goto DOWN before open*** " . reltimestr(reltime(time0))
	    endif
	    let projectVarDict 	= SaveProjectVariables()
" 	    let projectScript	= SaveProjectVariables("g:atp_ProjectLocalVariables")
" 	    let atp_ProjectScript 	= [ exists("g:atp_ProjectScript") ? g:atp_ProjectScript : b:atp_ProjectScript, exists("g:atp_ProjectScript") ] 
" 	    let g:atp_ProjectScript 	= 0
	    if empty(swapfile) || bufexists(next_branch)
		if g:atp_debugRS >= 2
		silent echo "Alternate (before open) " . bufname("#")
		endif
		silent! execute open
	    else
		echoerr "Recursive Search: swap file: " . swapfile . " exists. Aborting." 
		return
	    endif
	    if g:atp_debugRS >= 2
		silent echo "TIME ***goto DOWN after open*** " . reltimestr(reltime(time0))
	    endif
" 	    call RestoreProjectVariables(projectScript)
" 	    if atp_ProjectScript[1]
" 		let g:atp_ProjectScript = atp_ProjectScript[0]
" 	    else
" 		unlet g:atp_ProjectScript
" 	    endif
	    call RestoreProjectVariables(projectVarDict)
	    if g:atp_debugRS >= 2
		silent echo "TIME ***goto DOWN restore project variables *** " . reltimestr(reltime(time0))
	    endif

" 	    call cursor(g:ATP_branch_line, 1)
	    if flags_supplied !~# 'b'
		keepjumps call search('\m\\input\s*{[^}]*}', 'e', line(".")) 
	    endif

		if g:atp_debugRS
		silent echo "In lower branch:       " . g:ATP_branch . " branch_line=" . g:ATP_branch_line. " POS " . line(".") . ":" . col(".") 
		silent echo "Open Command           '" . open . "'"
		silent echo "exist b:TreeOfFiles    " . exists("b:TreeOfFiles")
		silent echo "flags_supplied:        " . flags_supplied
		endif
		if g:atp_debugRS >= 2
		silent echo "Alternate (after open) " . bufname("#")
		endif

		if g:atp_debugRS > 1
		silent echo "TIME_END:               " . reltimestr(reltime(time0))
		endif

	    unlet g:ATP_branch
	    unlet g:ATP_branch_line
" 	    let flag	= flags_supplied =~ 'W' ? flags_supplied : flags_supplied . 'W'
	    if goto_s == 'DOWN'
		keepalt keepjumps call <SID>RecursiveSearch(main_file, a:start_file, "", tree_of_files, expand("%:p"), a:call_nr+1, a:wrap_nr, a:winsaveview, a:bufnr, a:strftime, vim_options, cwd, pattern, flags_supplied)
	    endif

	" when REJECT
	elseif goto_s == 'REJECT'
	    echohl ErrorMsg
	    echomsg "[ATP:] pattern not found"
	    echohl Normal

	    if g:atp_debugRS > 1
		silent echo "TIME_END:" . reltimestr(reltime(time0))
	    endif

" 	    restore the window and buffer!
" 		it is better to remember bufnumber
	    silent execute "keepjumps keepalt edit #" . a:bufnr
	    call winrestview(a:winsaveview)

		if g:atp_debugRS
		silent echo ""
		redir END
		endif

	    " restore vim options 
	    if a:vim_options != { 'no_options' : 'no_options' }
		for option in keys(a:vim_options)
		    execute "let &l:".option."=".a:vim_options[option]
		endfor
	    endif
	    exe "lcd " . fnameescape(cwd)
" 	    syntax enable
	    filetype on
	    filetype detect

	    return

	" when ERROR
	elseif
	    echohl ErrorMsg
	    echomsg "[ATP:] this is a bug in ATP."
	    echohl
	    
	    " restore vim options 
	    if a:vim_options != { 'no_options' : 'no_options' }
		for option in keys(a:vim_options)
		    execute "let &l:".option."=".a:vim_options[option]
		endfor
	    endif
	    exe "lcd " . fnameescape(cwd)
" 	    syntax enable
	    filetype on
	    filetype detect

	    " restore the window and buffer!
	    silent execute "keepjumps keepalt edit #" . a:bufnr
	    call winrestview(a:winsaveview)

	    return 
	endif
endfunction
catch /E127:/  
endtry
" }}}

" User interface to s:RecursiveSearch function.
" s:GetSearchArgs {{{
" This functionn returns arguments from <q-args> - a list [ pattern, flag ]
" It allows to pass arguments to s:Search in a similar way to :vimgrep, :ijump, ... 
function! s:GetSearchArgs(Arg,flags)
    if a:Arg =~ '^\/'
	let pattern 	= matchstr(a:Arg, '^\/\zs.*\ze\/')
	let flag	= matchstr(a:Arg, '\/.*\/\s*\zs['.a:flags.']*\ze\s*$')
    elseif a:Arg =~ '^\i' && a:Arg !~ '^\w'
	let pattern 	= matchstr(a:Arg, '^\(\i\)\zs.*\ze\1')
	let flag	= matchstr(a:Arg, '\(\i\).*\1\s*\zs['.a:flags.']*\ze\s*$')
    else
	let pattern	= matchstr(a:Arg, '^\zs\S*\ze')
	let flag	= matchstr(a:Arg, '^\S*\s*\zs['.a:flags.']*\ze\s*$')
    endif
    return [ pattern, flag ]
endfunction
"}}}
" {{{ Search()
try
function! Search(Bang, Arg)

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    let [ pattern, flag ] = s:GetSearchArgs(a:Arg, 'bceswW')

    if pattern == ""
	echohl ErrorMsg
	echomsg "[ATP:] enclose the pattern with /.../"
	echohl Normal
	return
    endif

    if a:Bang == "!" || !exists("b:TreeOfFiles")
	call <SID>RecursiveSearch(atp_MainFile, expand("%:p"), 'make_tree', '', expand("%:p"), 1, 1, winsaveview(), bufnr("%"), reltime(), { 'no_options' : 'no_options' }, 'no_cwd', pattern, flag)
    else
	call <SID>RecursiveSearch(atp_MainFile, expand("%:p"), '', deepcopy(b:TreeOfFiles),  expand("%:p"), 1, 1, winsaveview(), bufnr("%"), reltime(), { 'no_options' : 'no_options' }, 'no_cwd', pattern, flag)
    endif

endfunction
catch /E127: Cannot redefine function/  
endtry
" }}}

function! ATP_ToggleNn(...) " {{{
    let on	= ( a:0 >=1 ? ( a:1 == 'on'  ? 1 : 0 ) : !g:atp_mapNn )
    let g:on	= on
	if !on
	    silent! nunmap <buffer> n
	    silent! nunmap <buffer> N
	    silent! aunmenu LaTeX.Toggle\ Nn\ [on]
	    let g:atp_mapNn	= 0
	    nmenu 550.79 &LaTeX.Toggle\ &Nn\ [off]<Tab>:ToggleNn		:ToggleNn<CR>
	    imenu 550.79 &LaTeX.Toggle\ &Nn\ [off]<Tab>:ToggleNn		<Esc>:ToggleNn<CR>a
	    tmenu LaTeX.Toggle\ Nn\ [off] atp maps to n,N.
	    echomsg "[ATP:] vim nN maps"  
	else
	    silent! nmap <buffer> <silent> n    <Plug>RecursiveSearchn
	    silent! nmap <buffer> <silent> N    <Plug>RecursiveSearchN
	    silent! aunmenu LaTeX.Toggle\ Nn\ [off]
	    let g:atp_mapNn	= 1
	    nmenu 550.79 &LaTeX.Toggle\ &Nn\ [on]<Tab>:ToggleNn			:ToggleNn<CR>
	    imenu 550.79 &LaTeX.Toggle\ &Nn\ [on]<Tab>:ToggleNn			<Esc>:ToggleNn<CR>a
	    tmenu LaTeX.Toggle\ Nn\ [on] n,N vim normal commands.
	    echomsg "[ATP:] atp nN maps"
	endif
endfunction
function! SearchHistCompletion(ArgLead, CmdLine, CursorPos)
    let search_history=[]
    let hist_entry	= histget("search")
    let nr = 0
    while hist_entry != ""
	call add(search_history, hist_entry)
	let nr 		-= 1
	let hist_entry	=  histget("search", nr)
    endwhile
    
    return filter(search_history, "v:val =~# '^'.a:ArgLead")
endfunction
"}}}
"}}}

" These are only variables and front end functions for Bib Search Engine of ATP.
" Search engine is define in autoload/atplib.vim script library.
"{{{ BibSearch
"-------------SEARCH IN BIBFILES ----------------------
" This function counts accurence of a:keyword in string a:line, 
" there are two methods keyword is a string to find (a:1=0)or a pattern to
" match, the pattern used to is a:keyword\zs.* to find the place where to cut.
" DEBUG:
" command -buffer -nargs=* Count :echo atplib#count(<args>)

let g:bibentries=['article', 'book', 'booklet', 'conference', 'inbook', 'incollection', 'inproceedings', 'manual', 'mastertheosis', 'misc', 'phdthesis', 'proceedings', 'techreport', 'unpublished']


"{{{ variables
let g:bibmatchgroup		='String'
let g:defaultbibflags		= 'tabejsyu'
let g:defaultallbibflags	= 'tabejfsvnyPNSohiuHcp'
let b:lastbibflags		= g:defaultbibflags	" Set the lastflags variable to the default value on the startup.
let g:bibflagsdict=atplib#bibflagsdict
" These two variables were s:... but I switched to atplib ...
let g:bibflagslist		= keys(g:bibflagsdict)
let g:bibflagsstring		= join(g:bibflagslist,'')
let g:kwflagsdict={ 	  '@a' : '@article', 	
	    		\ '@b' : '@book\%(let\)\@<!', 
			\ '@B' : '@booklet', 	
			\ '@c' : '@in\%(collection\|book\)', 
			\ '@m' : '@misc', 	
			\ '@M' : '@manual', 
			\ '@p' : '@\%(conference\)\|\%(\%(in\)\?proceedings\)', 
			\ '@t' : '@\%(\%(master)\|\%(phd\)\)thesis', 
			\ '@T' : '@techreport', 
			\ '@u' : '@unpublished' }    

"}}}

" Front End Function
" {{{ BibSearch
"  There are three arguments: {pattern}, [flags, [choose]]
function! BibSearch(bang,...)
"     let pattern = a:0 >= 1 ? a:1 : ""
"     let flag	= a:0 >= 2 ? a:2 : ""
	
    let time=reltime()
	
    let Arg = ( a:0 >= 1 ? a:1 : "" )
    if Arg != ""
	let [ pattern, flag ] = s:GetSearchArgs(Arg, 'aetbjsynvpPNShouH@BcpmMtTulL')
    else
	let [ pattern, flag ] = [ "", ""] 
    endif

    let b:atp_LastBibPattern 	= pattern
    "     This cannot be set here.  It is set later by atplib#showresults function.
    "     let b:atp_LastBibFlags	= flag
    let @/			= pattern

    if g:atp_debugBS
	exe "redir! > ".g:atp_TempDir."/Bibsearch.log"
	silent! echo "==========BibSearch=========================="
	silent! echo "b:BibSearch_pattern=" . pattern
	silent! echo "b:BibSearch bang="    . a:bang
	silent! echo "b:BibSearch flag="    . flag	
	let g:BibSearch_pattern = pattern
	let g:BibSearch_bang	= a:bang
	let g:BibSearch_flag	= flag
	redir END
    endif

    if !exists("s:bibdict") || a:bang == "!"
	let s:bibdict={}
	if !exists("b:ListOfFiles") || !exists("b:TypeDict") || a:bang == "!"
	    call TreeOfFiles(b:atp_MainFile)
	endif
	for file in b:ListOfFiles
	    if b:TypeDict[file] == "bib"
		let s:bibdict[file]=readfile(file)
	    endif
	endfor
    endif
    let b:atp_BibFiles=keys(s:bibdict)
"     let g:bibdict=s:bibdict

    if has("python") && g:atp_bibsearch == "python"
	call atplib#showresults( atplib#searchbib_py(pattern, keys(s:bibdict), a:bang), flag, pattern, s:bibdict)
    else
	call atplib#showresults( atplib#searchbib(pattern, s:bibdict, a:bang), flag, pattern, s:bibdict)
    endif
    let g:time_BibSearch=reltimestr(reltime(time))
endfunction
nnoremap <silent> <Plug>BibSearchLast		:call BibSearch("", b:atp_LastBibPattern, b:atp_LastBibFlags)<CR>
" }}}
"}}}
endif "}}}

" Commands And Highlightgs:
" {{{
command! -buffer -bang -complete=customlist,SearchHistCompletion -nargs=* S 	:call Search(<q-bang>, <q-args>) | let v:searchforward = ( s:GetSearchArgs(<q-args>, 'bceswW')[1] =~# 'b' ? 0 : 1 )
nmap <buffer> <silent> <Plug>RecursiveSearchn 	:call <SID>RecursiveSearch(atplib#FullPath(b:atp_MainFile), expand("%:p"), (exists("b:TreeOfFiles") ? "" : "make_tree"), (exists("b:TreeOfFiles") ? b:TreeOfFiles : {}), expand("%:p"), 1, 1, winsaveview(), bufnr("%"), reltime(), { 'no_options' : 'no_options' }, 'no_cwd', @/, v:searchforward ? "" : "b") <CR>
nmap <buffer> <silent> <Plug>RecursiveSearchN 	:call <SID>RecursiveSearch(atplib#FullPath(b:atp_MainFile), expand("%:p"), (exists("b:TreeOfFiles") ? "" : "make_tree"), (exists("b:TreeOfFiles") ? b:TreeOfFiles : {}), expand("%:p"), 1, 1, winsaveview(), bufnr("%"), reltime(), { 'no_options' : 'no_options' }, 'no_cwd', @/, !v:searchforward ? "" : "b") <CR>

if g:atp_mapNn
" These two maps behaves now like n (N): after forward search n (N) acts as forward (backward), after
" backward search n acts as backward (forward, respectively).

    nmap <buffer> <silent> n		<Plug>RecursiveSearchn
    nmap <buffer> <silent> N		<Plug>RecursiveSearchN

    " Note: the final step if the mapps n and N are made is in s:LoadHistory 
endif

command! -buffer -bang 		LocalCommands					:call LocalCommands(1, "", <q-bang>)
command! -buffer -bang -nargs=* -complete=customlist,DsearchComp Dsearch	:call <SID>Dsearch(<q-bang>, <q-args>)
command! -buffer -nargs=? -complete=customlist,atplib#OnOffComp ToggleNn	:call ATP_ToggleNn(<f-args>)
command! -buffer -bang -nargs=* BibSearch					:call BibSearch(<q-bang>, <q-args>)

" Hilighlting:
hi link BibResultsFileNames 	Title	
hi link BibResultEntry		ModeMsg
hi link BibResultsMatch		WarningMsg
hi link BibResultsGeneral	Normal

hi link Chapter 		Normal	
hi link Section			Normal
hi link Subsection		Normal
hi link Subsubsection		Normal
hi link CurrentSection		WarningMsg
"}}}
" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
