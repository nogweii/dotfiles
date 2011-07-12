" Title:		Vim filetype plugin file
" Author:		Marcin Szamotulski
" Mailing List: 	atp-vim-list [AT] lists.sourceforge.net
" Do NOT DELETE the line just below, it is used by :UpdateATP (':help atp-:UpdateATP')
" Time Stamp: 12-06-11_18-10
" (but you can edit, if there is a reason for doing this. The format is dd-mm-yy_HH-MM)
" Language:	    tex
" Last Change: Mon Jun 06 10:00  2011 W
" GetLatestVimScripts: 2945 62 :AutoInstall: tex_atp.vim
" GetLatestVimScripts: 884 1 :AutoInstall: AutoAlign.vim
" Copyright Statement: 
" 	  This file is a part of Automatic Tex Plugin for Vim.
"
"     Automatic Tex Plugin for Vim is free software: you can redistribute it
"     and/or modify it under the terms of the GNU General Public License as
"     published by the Free Software Foundation, either version 3 of the
"     License, or (at your option) any later version.
" 
"     Automatic Tex Plugin for Vim is distributed in the hope that it will be
"     useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
"     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
"     General Public License for more details.
" 
"     You should have received a copy of the GNU General Public License along
"     with Automatic Tex Plugin for Vim.  If not, see <http://www.gnu.org/licenses/>.
"
"     This licence applies to all files shipped with Automatic Tex Plugin.

" Do not source ATP if g:no_atp is set
let reltime=reltime()
if exists("g:no_atp") && g:no_atp == 1
    finish
endif

let b:did_ftplugin	= 1

if !exists("g:atp_reload_functions")
	let g:atp_reload_functions = 0
endif
if !exists("g:atp_reload_variables")
	let g:atp_reload_variables = 0
endif

if &cpoptions =~ '<'
	echoerr "[ATP:] removing '<' from cpoptions"
	setl cpoptions-=<
endif

    " Execute the atprc file.
    " They override cached variables
    let s:atprc_file = globpath($HOME, '.atprc.vim', 1)
    " They override cached variables
    function! <SID>ReadATPRC()
	    if filereadable(s:atprc_file) && ( has("unix") || has("max") || has("macunix") )

		    " Note: in $HOME/.atprc file the user can set all the local buffer
		    " variables without using autocommands
		    "
		    " Note: it must be sourced at the begining because some options handle
		    " how atp will load (for example if we load history or not)
		    " It also should be run at the end if the user defines mapping that
		    " should be overwrite the ATP settings (this is done via
		    " autocommand).
		    let path = globpath($HOME, '.atprc.vim', 1)
		    execute 'source ' . fnameescape(path)

	    else
		    let path	= get(split(globpath(&rtp, "**/ftplugin/ATP_files/atprc.vim"), '\n'), 0, "")
		    if path != ""
			    execute 'source ' . fnameescape(path)
		    endif
	    endif
    endfunction

let reltime_project=reltime()
	" Source Project Script
	runtime ftplugin/ATP_files/project.vim
let g:source_time_project=reltimestr(reltime(reltime_project))

	" ATPRC file overwrites project settings
	" (if the user put sth in atprc file, it means that he wants this globbaly) 
let reltime_atprc_begin=reltime()
	call <SID>ReadATPRC()
let g:source_time_atprc_begin=reltimestr(reltime(reltime_atprc_begin))

	" Functions needed before setting options.
	runtime ftplugin/ATP_files/common.vim

	" Options, global and local variables, autocommands.
let reltime_options=reltime()
	runtime ftplugin/ATP_files/options.vim
let g:source_time_options=reltimestr(reltime(reltime_options))


let reltime_compiler=reltime()
	" Compilation related stuff.
	runtime ftplugin/ATP_files/compiler.vim
let g:source_time_compiler=reltimestr(reltime(reltime_compiler))

let reltime_LatexBox=reltime()
" 	let compiler_file = findfile('compiler/tex_atp.vim', &rtp)
" 	if compiler_file
" 		execute 'source ' 	. fnameescape(compiler_file)
" 	endif

	" LatexBox addons (by D.Munger, with some modifications).
	if g:atp_LatexBox

		runtime ftplugin/ATP_files/LatexBox_common.vim
		runtime ftplugin/ATP_files/LatexBox_complete.vim
		runtime ftplugin/ATP_files/LatexBox_motion.vim
		runtime ftplugin/ATP_files/LatexBox_latexmk.vim

	endif
let g:source_time_LatexBox=reltimestr(reltime(reltime_LatexBox))

let reltime_motion=reltime()
	runtime ftplugin/ATP_files/motion.vim

	runtime ftplugin/ATP_files/search.vim

	runtime ftplugin/ATP_files/various.vim
let g:source_time_motion=reltimestr(reltime(reltime_motion))

let reltime_maps=reltime()
	" Source maps and menu files.
	runtime ftplugin/ATP_files/mappings.vim

	if g:atp_LatexBox

		" LatexBox mappings.
		runtime ftplugin/ATP_files/LatexBox_mappings.vim
			
	endif
let g:source_time_maps=reltimestr(reltime(reltime_maps))

let reltime_abbrev=reltime()
	" Source abbreviations.
	runtime ftplugin/ATP_files/abbreviations.vim
let g:source_time_abbrev=reltimestr(reltime(reltime_abbrev))

let reltime_menu=reltime()
	" The menu.
	runtime ftplugin/ATP_files/menu.vim
let g:source_time_menu=reltimestr(reltime(reltime_menu))

	" Help functions.
	runtime ftplugin/ATP_files/helpfunctions.vim

let reltime_atprc_end=reltime()
	" Read ATPRC once again (to set mapps).
	call <SID>ReadATPRC()
let g:source_time_atprc_end=reltimestr(reltime(reltime_atprc_end))

let g:source_time=reltimestr(reltime(reltime))
command! SourceTime echo g:source_time_project." project\n".
	    \ g:source_time_atprc_begin." begin\n".
	    \ g:source_time_options." options\n".
	    \ g:source_time_compiler." compiler\n".
	    \ g:source_time_LatexBox." LatexBox\n".
	    \ g:source_time_motion." motion\n".
	    \ g:source_time_maps." maps\n".
	    \ g:source_time_abbrev." abbrev\n".
	    \ g:source_time_menu." menu\n".
	    \ g:source_time_atprc_end." atprc_end\n  ".
	    \ string(str2float(g:source_time_atprc_end)+str2float(g:source_time_menu)+str2float(g:source_time_abbrev)+
	    \ str2float(g:source_time_maps)+str2float(g:source_time_motion)+str2float(g:source_time_LatexBox)+
	    \ str2float(g:source_time_compiler)+str2float(g:source_time_options)+str2float(g:source_time_atprc_begin)+str2float(g:source_time_project))." sum\n".
	    \ g:source_time." overall time"
" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
