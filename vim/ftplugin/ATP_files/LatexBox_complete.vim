" Author:	David Mnuger (latexbox vim plugin)
" Maintainer:	Marcin Szamotulski
" Note:		This file is a part of Automatic Tex Plugin for Vim.
" Email:	mszamot [AT] gmail [DOT] com
" Language:	tex
" Last Change:

let s:sourced = exists("s:sourced") ? 1 : 0
if s:sourced && !g:atp_reload_functions
    finish
endif

let s:atp_MainFile	= ( g:atp_RelativePath ? globpath(b:atp_ProjectDir, fnamemodify(b:atp_MainFile, ":t")) : b:atp_MainFile )

" latex-box/complete.vim
" <SID> Wrap {{{
function! s:GetSID()
	return matchstr(expand('<sfile>'), '\zs<SNR>\d\+_\ze.*$')
endfunction
let s:SID = s:GetSID()
call extend(g:atp_compiler_SID,{ fnamemodify(expand('<sfile>'),':t') : s:SID })
" a:1 is the file where the function is defined. 
function! s:SIDWrap(func,...)
	return s:SID . a:func
endfunction
" }}}


" Omni Completion {{{

let s:completion_type = ''

function! LatexBox_Complete(findstart, base)
	if a:findstart
		" return the starting position of the word
		let line = getline('.')
		let pos = col('.') - 1
		while pos > 0 && line[pos - 1] !~ '\\\|{'
			let pos -= 1
		endwhile

		let line_start = line[:pos-1]
		if line_start =~ '\C\\begin\_\s*{$'
			let s:completion_type = 'begin'
		elseif line_start =~ '\C\\end\_\s*{$'
			let s:completion_type = 'end'
		elseif line_start =~ g:LatexBox_ref_pattern . '$'
			let s:completion_type = 'ref'
		elseif line_start =~ g:LatexBox_cite_pattern . '$'
			let s:completion_type = 'bib'
			" check for multiple citations
			let pos = col('.') - 1
			while pos > 0 && line[pos - 1] !~ '{\|,'
				let pos -= 1
			endwhile
		else
			let s:completion_type = 'command'
			if line[pos - 1] == '\'
				let pos -= 1
			endif
		endif
		return pos
	else
		" return suggestions in an array
		let suggestions = []

		if s:completion_type == 'begin'
			" suggest known environments
			for entry in g:LatexBox_completion_environments
				if entry.word =~ '^' . escape(a:base, '\')
					if g:LatexBox_completion_close_braces && !s:NextCharsMatch('^}')
						" add trailing '}'
						let entry = copy(entry)
						let entry.abbr = entry.word
						let entry.word = entry.word . '}'
					endif
					call add(suggestions, entry)
				endif
			endfor
		elseif s:completion_type == 'end'
			" suggest known environments
			let env = LatexBox_GetCurrentEnvironment()
			if env != ''
				if g:LatexBox_completion_close_braces && !s:NextCharsMatch('^\s*[,}]')
					call add(suggestions, {'word': env . '}', 'abbr': env})
				else
					call add(suggestions, env)
				endif
			endif
		elseif s:completion_type == 'command'
			" suggest known commands
			for entry in g:LatexBox_completion_commands
				if entry.word =~ '^' . escape(a:base, '\')
					" do not display trailing '{'
					if entry.word =~ '{'
						let entry.abbr = entry.word[0:-2]
					endif
					call add(suggestions, entry)
				endif
			endfor
		elseif s:completion_type == 'ref'
			let suggestions = s:CompleteLabels(a:base)
		elseif s:completion_type == 'bib'
			" suggest BibTeX entries
			let suggestions = LatexBox_BibComplete(a:base)
		endif
		if !has('gui_running')
			redraw!
		endif
		return suggestions
	endif
endfunction
" }}}


" BibTeX search {{{

" find the \bibliography{...} commands
" the optional argument is the file name to be searched
function! LatexBox_kpsewhich(file)
	let old_dir = getcwd()
	execute 'lcd ' . LatexBox_GetTexRoot()
	redir => out
	silent execute '!kpsewhich ' . a:file
	redir END

	let out = split(out, "\<NL>")[-1]
	let out = substitute(out, '\r', '', 'g')
	let out = glob(fnamemodify(out, ':p'), 1)
	
	execute 'lcd ' . fnameescape(old_dir)

	return out
endfunction

function! s:FindBibData(...)

	if a:0 == 0
		let file = ( g:atp_RelativePath ? globpath(b:atp_ProjectDir, fnamemodify(b:atp_MainFile, ":t")) : b:atp_MainFile )
	else
		let file = a:1
	endif

	if empty(glob(file, 1))
		return ''
		endif

	let lines = readfile(file)

	let bibdata_list = []

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\C\\bibliography\s*{[^}]\+}'''),
				\ 'matchstr(v:val, ''\C\\bibliography\s*{\zs[^}]\+\ze}'')')

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\C\\\%(input\|include\)\s*{[^}]\+}'''),
				\ 's:FindBibData(LatexBox_kpsewhich(matchstr(v:val, ''\C\\\%(input\|include\)\s*{\zs[^}]\+\ze}'')))')

	let bibdata_list +=
				\ map(filter(copy(lines), 'v:val =~ ''\C\\\%(input\|include\)\s\+\S\+'''),
				\ 's:FindBibData(LatexBox_kpsewhich(matchstr(v:val, ''\C\\\%(input\|include\)\s\+\zs\S\+\ze'')))')

	let bibdata = join(bibdata_list, ',')

	return bibdata
endfunction

let s:bstfile = expand('<sfile>:p:h') . '/vimcomplete'

function! LatexBox_BibSearch(regexp)

	" find bib data
    let bibdata = s:FindBibData()
    if bibdata == ''
	echomsg '[ATP:] error: no \bibliography{...} command found'
	return
    endif

    " write temporary aux file
    let tmpbase = b:atp_ProjectDir  . '/_LatexBox_BibComplete'
    let auxfile = tmpbase . '.aux'
    let bblfile = tmpbase . '.bbl'
    let blgfile = tmpbase . '.blg'

    call writefile(['\citation{*}', '\bibstyle{' . s:bstfile . '}', '\bibdata{' . bibdata . '}'], auxfile)

    let atp_MainFile	= atplib#FullPath(b:atp_MainFile)
    silent execute '! cd ' shellescape(fnamemodify(atp_MainFile,":h")) .
				\ ' ; bibtex -terse ' . fnamemodify(auxfile, ':t') . ' >/dev/null'

    let res = []
    let curentry = ''

	let lines = split(substitute(join(readfile(bblfile), "\n"), '\n\n\@!\(\s\=\)\s*\|{\|}', '\1', 'g'), "\n")
			
    for line in filter(lines, 'v:val =~ a:regexp')
		let matches = matchlist(line, '^\(.*\)||\(.*\)||\(.*\)||\(.*\)||\(.*\)')
            if !empty(matches) && !empty(matches[1])
                call add(res, {'key': matches[1], 'type': matches[2],
							\ 'author': matches[3], 'year': matches[4], 'title': matches[5]})
	    endif
    endfor

	call delete(auxfile)
	call delete(bblfile)
	call delete(blgfile)

	return res
endfunction
" }}}

" BibTeX completion {{{
function! LatexBox_BibComplete(regexp)

	" treat spaces as '.*' if needed
	if g:LatexBox_bibtex_wild_spaces
		"let regexp = substitute(a:regexp, '\s\+', '.*', 'g')
		let regexp = '.*' . substitute(a:regexp, '\s\+', '\\\&.*', 'g')
	else
		let regexp = a:regexp
	endif

    let res = []
    for m in LatexBox_BibSearch(regexp)

        let w = {'word': m['key'],
					\ 'abbr': '[' . m['type'] . '] ' . m['author'] . ' (' . m['year'] . ')',
					\ 'menu': m['title']}

		" close braces if needed
		if g:LatexBox_completion_close_braces && !s:NextCharsMatch('^\s*[,}]')
			let w.word = w.word . '}'
		endif

	call add(res, w)
    endfor
    return res
endfunction
" }}}

" Complete Labels {{{
" the optional argument is the file name to be searched
function! s:CompleteLabels(regex, ...)

	if a:0 == 0
	    let atp_MainFile = atplib#FullPath(b:atp_MainFile)
		let file = fnamemodify(atp_MainFile, ":r") . ".aux" 
	else
		let file = a:1
	endif

	if empty(glob(file, 1))
		return []
	endif

	let suggestions = []

	" search for the target equation number
	for line in filter(readfile(file), 'v:val =~ ''^\\newlabel{\|^\\@input{''')

		echomsg "[ATP:] matching line: " . line

		" search for matching label
		let matches = matchlist(line, '^\\newlabel{\(' . a:regex . '[^}]*\)}{{\([^}]*\)}{\([^}]*\)}.*}')

		if empty(matches)
		    " also try to match label and number
		    let regex_split = split(a:regex)
		    if len(regex_split) > 1
			let base = regex_split[0]
			let number = escape(join(regex_split[1:], ' '), '.')
			let matches = matchlist(line, '^\\newlabel{\(' . base . '[^}]*\)}{{\(' . number . '\)}{\([^}]*\)}.*}')
		    endif
		endif

		if empty(matches)
			" also try to match number
			let matches = matchlist(line, '^\\newlabel{\([^}]*\)}{{\(' . escape(a:regex, '.') . '\)}{\([^}]*\)}.*}')
		endif

		if !empty(matches)

			let entry = {'word': matches[1], 'menu': '(' . matches[2] . ') [p.' . matches[3] . ']'}

			if g:LatexBox_completion_close_braces && !s:NextCharsMatch('^\s*[,}]')
				" add trailing '}'
				let entry = copy(entry)
				let entry.abbr = entry.word
				let entry.word = entry.word . '}'
			endif
        call add(suggestions, entry)
      endif

		" search for included files
		let included_file = matchstr(line, '^\\@input{\zs[^}]*\ze}')
		if included_file != ''
			let included_file = LatexBox_kpsewhich(included_file)
			call extend(suggestions, s:CompleteLabels(a:regex, included_file))
		endif
	endfor

	return suggestions

endfunction
" }}}

" Change Environment {{{
function! s:ChangeEnvPrompt()

	let [env, lnum, cnum, lnum2, cnum2] = LatexBox_GetCurrentEnvironment(1)

	let new_env = input('change ' . env . ' for: ', '', 'customlist,' . s:SIDWrap('GetEnvironmentList'))
	if empty(new_env)
		return
	endif

	if new_env == '\[' || new_env == '['
		let begin = '\['
		let end = '\]'
	elseif new_env == '\(' || new_env == '('
		let begin = '\('
		let end = '\)'
	else
		let l:begin = '\begin{' . new_env . '}'
		let l:end = '\end{' . new_env . '}'
	endif
	
	if env == '\[' || env == '\('
		let line = getline(lnum2)
		let line = strpart(line, 0, cnum2 - 1) . l:end . strpart(line, cnum2 + 1)
		call setline(lnum2, line)

		let line = getline(lnum)
		let line = strpart(line, 0, cnum - 1) . l:begin . strpart(line, cnum + 1)
		call setline(lnum, line)
	else
		let line = getline(lnum2)
		let line = strpart(line, 0, cnum2 - 1) . l:end . strpart(line, cnum2 + len(env) + 5)
		call setline(lnum2, line)

		let line = getline(lnum)
		let line = strpart(line, 0, cnum - 1) . l:begin . strpart(line, cnum + len(env) + 7)
		call setline(lnum, line)
	endif

endfunction

function! s:GetEnvironmentList(lead, cmdline, pos)
	let suggestions = []
	
	if !exists("b:atp_LocalEnvironments")
	    LocalCommands
	elseif has("python")
	    LocalCommands
	endif
	let l:completion_list=atplib#Extend(g:atp_Environments,b:atp_LocalEnvironments)

	for entry in l:completion_list
" 		let env = entry.word
		if entry =~ '^' . a:lead
			call add(suggestions, entry)
		endif
	endfor

	if len(suggestions) > 5
	    call sort(suggestions)
	endif

	return suggestions
endfunction
" }}}

" Next Charaters Match {{{
function! s:NextCharsMatch(regex)
	let rest_of_line = strpart(getline('.'), col('.') - 1)
	return rest_of_line =~ a:regex
endfunction
" }}}

" Mappings {{{
nmap <silent> <Plug>LatexChangeEnv			:call <SID>ChangeEnvPrompt()<CR>
" }}}

