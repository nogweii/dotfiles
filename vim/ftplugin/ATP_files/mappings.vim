" Author:	Marcin Szmotulski
" Description:  This file contains mappings defined by ATP.
" Note:		This file is a part of Automatic Tex Plugin for Vim.
" Language:	tex
" Last Change: Sun Jun 12 04:00  2011 W

" Add maps, unless the user didn't want them.
if exists("g:no_plugin_maps") && g:no_plugin_maps ||
	    \ exists("g:no_atp_maps") 	&& g:no_atp_maps ||
	    \ exists("g:no_".&l:filetype."_maps") && g:no_{&l:filetype}_maps
    finish
endif

" Try to be cpoptions compatible:
if &l:cpoptions =~# "B"
    let s:backslash="\\"
    let s:bbackslash="\\\\"
else
    let s:backslash="\\\\"
    let s:bbackslash="\\\\\\\\"
endif
let g:backslash=s:backslash


" Commands to library functions (autoload/atplib.vim)

" <c-c> in insert mode doesn't trigger InsertLeave autocommands
" this fixes this.
if g:atp_IMapCC
    imap <silent> <buffer> <C-c> <C-[>
endif

if has("gui")
    if &l:cpoptions =~# "B"
	if g:atp_cmap_space
	    cmap <buffer> <expr> <space> 	( g:atp_cmap_space && getcmdtype() =~ '[\/?]' ? '\_s\+' : ' ' )
	endif
	cmap <expr> <buffer> <C-Space> getcmdtype() =~ '[?/]' ? '\_s\+' : ' ' 
	cmap <expr> <buffer> <C-_> getcmdtype() =~ '[?/]' ? '\_s\+' : ' '
    else
	if g:atp_cmap_space
	    cmap <buffer> <expr> <space> 	( g:atp_cmap_space && getcmdtype() =~ '[\\/?]' ? '\\_s\\+' : ' ' )
	endif
	cmap <expr> <buffer> <C-Space> getcmdtype() =~ '[?/]' ? '\\_s\\+' : ' '
	cmap <expr> <buffer> <C-_> getcmdtype() =~ '[?/]' ? '\\_s\\+' : ' '
    endif
else
    if &l:cpoptions =~# "B"
	if g:atp_cmap_space
	    cmap <buffer> <expr> <space> 	( g:atp_cmap_space && getcmdtype() =~ '[\/?]' ? '\_s\+' : ' ' )
	endif
	cmap <expr> <buffer> <C-@> getcmdtype() =~ '[?/]' ? '\_s\+' : ' '
	cmap <expr> <buffer> <C-_> getcmdtype() =~ '[?/]' ? '\_s\+' : ' '
    else
	if g:atp_cmap_space
	    cmap <buffer> <expr> <space> 	( g:atp_cmap_space && getcmdtype() =~ '[\\/?]' ? '\\_s\\+' : ' ' )
	endif
	cmap <expr> <buffer> <C-@> getcmdtype() =~ '[?/]' ? '\\_s\\+' : ' '
	cmap <expr> <buffer> <C-_> getcmdtype() =~ '[?/]' ? '\\_s\\+' : ' '
    endif
endif
if maparg("<F2>", "n") == ""
    nmap <buffer> <F2>	:echo ATP_ToggleSpace()<CR>
endif

command! -buffer -bang -nargs=* FontSearch	:call atplib#FontSearch(<q-bang>, <f-args>)
command! -buffer -bang -nargs=* FontPreview	:call atplib#FontPreview(<q-bang>,<f-args>)
command! -buffer -nargs=1 -complete=customlist,atplib#Fd_completion OpenFdFile	:call atplib#OpenFdFile(<f-args>) 
command! -buffer -nargs=* CloseLastEnvironment	:call atplib#CloseLastEnvironment(<f-args>)
command! -buffer 	  CloseLastBracket	:call atplib#CloseLastBracket()

" MAPS:

if !hasmapto("\"SSec") && !hasmapto("'SSec")
    exe "nmap <buffer> <silent>	".g:atp_goto_section_leader."S		:<C-U>keepjumps exe v:count1.\"SSec\"<CR>"
endif
if !hasmapto("\"Sec") && !hasmapto("'Sec")
    exe "nmap <buffer> <silent>	".g:atp_goto_section_leader."s		:<C-U>keepjumps exe v:count1.\"Sec\"<CR>"
endif
if !hasmapto("\"Chap") && !hasmapto("'Chap")
    exe "nmap <buffer> <silent>	".g:atp_goto_section_leader."c		:<C-U>keepjumps exe v:count1.\"Chap\"<CR>"
endif
if !hasmapto("\"Part") && !hasmapto("'Part")
    exe "nmap <buffer> <silent>	".g:atp_goto_section_leader."p		:<C-U>keepjumps exe v:count1.\"Part\"<CR>"
endif

if g:atp_MapCommentLines    
    if !hasmapto("<Plug>CommentLines", "n")
	exe "nmap <buffer> <silent> ".g:atp_map_Comment."	<Plug>CommentLines"
    endif
    if !hasmapto("<Plug>CommentLines", "v")
	exe "vmap <buffer> <silent> ".g:atp_map_Comment."	<Plug>CommentLines"
    endif
    if !hasmapto("<Plug>UnCommentLines", "n")
	exe "nmap <buffer> <silent> ".g:atp_map_UnComment."	<Plug>UnCommentLines"
    endif
    if !hasmapto("<Plug>UnCommentLines", "v")
	exe "vmap <buffer> <silent> ".g:atp_map_UnComment."	<Plug>UnCommentLines"
    endif
endif

if !hasmapto("<Plug>SyncTexKeyStroke", "n")
    nmap <buffer> <silent> t 			<Plug>SyncTexKeyStroke
endif
if !hasmapto("<LeftMouse><Plug>SyncTexMouse", "n")
    nmap <buffer> <silent> <S-LeftMouse> 	<LeftMouse><Plug>SyncTexMouse
endif

if !hasmapto(":SkipCommentForward<CR>", 'n')
    nmap <buffer> <silent> ]*	:SkipCommentForward<CR> 
    nmap <buffer> <silent> gc	:SkipCommentForward<CR>
endif
if !hasmapto(":SkipCommentForward<CR>", 'o')
    omap <buffer> <silent> ]*	:SkipCommentForward<CR> 
    omap <buffer> <silent> gc	:SkipCommentForward<CR>
endif
if !hasmapto("<Plug>SkipCommentForward", 'v')
    vmap <buffer> <silent> ]*	<Plug>SkipCommentForward
    vmap <buffer> <silent> gc	<Plug>SkipCommentForward
endif

if !hasmapto("<Plug>SkipCommentBackward<CR>", 'n')
    nmap <buffer> <silent> [*	:SkipCommentBackward<CR> 
    nmap <buffer> <silent> gC	:SkipCommentBackward<CR>
endif
if !hasmapto("<Plug>SkipCommentBackward<CR>", 'o')
    omap <buffer> <silent> [*	:SkipCommentBackward<CR> 
    omap <buffer> <silent> gC	:SkipCommentBackward<CR>
endif
if !hasmapto("<Plug>SkipCommentBackward", 'v')
    vmap <buffer> <silent> gC	<Plug>SkipCommentBackward
    vmap <buffer> <silent> [*	<Plug>SkipCommentBackward
endif

if !hasmapto(":NInput<CR>")
    execute "nmap <silent> <buffer> ".g:atp_map_forward_motion_leader."i	:NInput<CR>"
    execute "nmap <silent> <buffer> ".g:atp_map_forward_motion_leader."gf	:NInput<CR>"
endif

if !hasmapto(":PInput<CR>")
    execute "nmap <silent> <buffer> ".g:atp_map_backward_motion_leader."i	:PInput<CR>"
    execute "nmap <silent> <buffer> ".g:atp_map_backward_motion_leader."gf	:PInput<CR>"
endif

" Syntax motions:
" imap <C-j> <Plug>TexSyntaxMotionForward
" imap <C-k> <Plug>TexSyntaxMotionBackward
" nmap <C-j> <Plug>TexSyntaxMotionForward
" nmap <C-k> <Plug>TexSyntaxMotionBackward

if !hasmapto("<Plug>TexJMotionForward", 'i')
    imap <C-j> <Plug>TexJMotionForward
endif
if !hasmapto("<Plug>TexJMotionForward", 'n')
    nmap <C-j> <Plug>TexJMotionForward
endif
if !hasmapto("<Plug>TexJMotionBackward", 'i')
    imap <C-k> <Plug>TexJMotionBackward
endif
if !hasmapto("<Plug>TexJMotionBackward", 'n')
    nmap <C-k> <Plug>TexJMotionBackward
endif

" Repair: } and { 
if g:atp_map_forward_motion_leader == "}"
    noremap <silent> <buffer> }} }
endif
if g:atp_map_backward_motion_leader == "{"
    noremap <silent> <buffer> {{ {
endif
" Repair: > and >> (<, <<) operators:
if g:atp_map_forward_motion_leader == ">"
    nnoremap <buffer> <silent> >>  :<C-U>exe "normal! ".v:count1.">>"<CR>
    vnoremap <buffer> <silent> >>  :<C-U>exe "'<,'>normal! v".v:count1.">>"<CR>
endif
if g:atp_map_backward_motion_leader == "<"
    nnoremap <buffer> <silent> <<  :<C-U>exe "normal! ".v:count1."<<"<CR>
    vnoremap <buffer> <silent> <<  :<C-U>exe "'<,'>normal! v".v:count1."<<"<CR>
endif

if !hasmapto("<Plug>GotoNextSubSection", 'n')
    execute "nmap <silent> <buffer> ".g:atp_map_forward_motion_leader."S 	<Plug>GotoNextSubSection"
endif
if !hasmapto("<Plug>vGotoNextSubSection", 'v')
    execute "vmap <silent> <buffer> ".g:atp_map_forward_motion_leader."S	<Plug>vGotoNextSubSection"
endif
if !hasmapto("<Plug>GotoPreviousSubSection", 'n')
    execute "nmap <silent> <buffer> ".g:atp_map_backward_motion_leader."S 	<Plug>GotoPreviousSubSection"
endif
if !hasmapto("<Plug>vGotoPreviousSubSection", 'v')
    execute "vmap <silent> <buffer> ".g:atp_map_backward_motion_leader."S 	<Plug>vGotoPreviousSubSection"
endif
if !hasmapto("<Plug>GotoNextSection", 'n')
    execute "nmap <silent> <buffer> ".g:atp_map_forward_motion_leader."s 	<Plug>GotoNextSection"
endif
if !hasmapto("<Plug>vGotoNextSection", 'v')
    execute "vmap <silent> <buffer> ".g:atp_map_forward_motion_leader."s	<Plug>vGotoNextSection"
endif
if !hasmapto("<Plug>GotoPreviousSection", 'n')
    execute "nmap <silent> <buffer> ".g:atp_map_backward_motion_leader."s 	<Plug>GotoPreviousSection"
endif
if !hasmapto("<Plug>vGotoPreviousSection", 'v')
    execute "vmap <silent> <buffer> ".g:atp_map_backward_motion_leader."s 	<Plug>vGotoPreviousSection"
endif
if !( g:atp_map_forward_motion_leader == "]" && &l:diff )
    if !hasmapto("<Plug>GotoNextChapter", 'n')
	execute "nmap <silent> <buffer> ".g:atp_map_forward_motion_leader."c 	<Plug>GotoNextChapter"
    endif
    if !hasmapto("<Plug>vGotoNextChapter", 'v')
	execute "vmap <silent> <buffer> ".g:atp_map_forward_motion_leader."c 	<Plug>vGotoNextChapter"
    endif
endif
if !( g:atp_map_backward_motion_leader == "]" && &l:diff )
    if !hasmapto("<Plug>GotoPreviousChapter", 'n')
	execute "nmap <silent> <buffer> ".g:atp_map_backward_motion_leader."c 	<Plug>GotoPreviousChapter"
    endif
    if !hasmapto("<Plug>vGotoPreviousChapter", 'v')
	execute "vmap <silent> <buffer> ".g:atp_map_backward_motion_leader."c 	<Plug>vGotoPreviousChapter"
    endif
endif
if !hasmapto("<Plug>GotoNextPart", 'n')
    execute "nmap <silent> <buffer> ".g:atp_map_forward_motion_leader."p 	<Plug>GotoNextPart"
endif
if !hasmapto("<Plug>vGotoNextPart", 'v')
    execute "vmap <silent> <buffer> ".g:atp_map_forward_motion_leader."p 	<Plug>vGotoNextPart"
endif
if !hasmapto("<Plug>GotoPreviousPart", "n")
    execute "nmap <silent> <buffer> ".g:atp_map_backward_motion_leader."p 	<Plug>GotoPreviousPart"
endif
if !hasmapto("<Plug>vGotoPreviousPart", 'v')
    execute "vmap <silent> <buffer> ".g:atp_map_backward_motion_leader."p 	<Plug>vGotoPreviousPart"
endif

if !hasmapto("<Plug>GotoNextEnvironment")
    execute "map <silent> <buffer> ".g:atp_map_forward_motion_leader."e		<Plug>GotoNextEnvironment"
endif
if !hasmapto("<Plug>JumptoNextEnvironment")
    execute "map <silent> <buffer> ".g:atp_map_forward_motion_leader."E		<Plug>JumptoNextEnvironment"
endif
if !hasmapto("<Plug>GotoPreviousEnvironment")
    execute "map <silent> <buffer> ".g:atp_map_backward_motion_leader."e	<Plug>GotoPreviousEnvironment"
endif
if !hasmapto("<Plug>JumptoPreviousEnvironment")
    execute "map <silent> <buffer> ".g:atp_map_backward_motion_leader."E 	<Plug>JumptoPreviousEnvironment"
endif
if !hasmapto("<Plug>GotoNextMath")
    execute "map <silent> <buffer> ".g:atp_map_forward_motion_leader."m		<Plug>GotoNextMath"
endif
if !hasmapto("<Plug>GotoPreviousMath")
    execute "map <silent> <buffer> ".g:atp_map_backward_motion_leader."m	<Plug>GotoPreviousMath"
endif
if !hasmapto("<Plug>GotoNextDisplayedMath")
    execute "map <silent> <buffer> ".g:atp_map_forward_motion_leader."M		<Plug>GotoNextDisplayedMath"
endif
if !hasmapto("<Plug>GotoPreviousDisplayedMath")
    execute "map <silent> <buffer> ".g:atp_map_backward_motion_leader."M	<Plug>GotoPreviousDisplayedMath"
endif

" Goto File Map:
if has("path_extra") && !hasmapto(" GotoFile(", 'n')
	nnoremap <buffer> <silent> gf		:call GotoFile("", "")<CR>
endif

if !exists("g:atp_no_tab_map") || g:atp_no_tab_map == 0
    "Default Completion Maps:
    if !hasmapto("<C-R>=atplib#TabCompletion(1)<CR>", 'i')
	imap <silent> <buffer> <Tab> 		<C-R>=atplib#TabCompletion(1)<CR>
    endif
    if !hasmapto("<C-R>=atplib#TabCompletion(0)<CR>", 'i')
	imap <silent> <buffer> <S-Tab> 		<C-R>=atplib#TabCompletion(0)<CR>
    endif
" 	if !hasmapto("atplib#TabCompletion(1,1)<CR>", 'n')
" 	    nmap <silent> <buffer> <Tab>		:call atplib#TabCompletion(1,1)<CR>
" 	endif
    if !hasmapto("atplib#TabCompletion(0,1)<CR>", 'i')
	nnoremap <silent> <buffer> <S-Tab>	:call atplib#TabCompletion(0,1)<CR> 
    endif
    if !hasmapto(":WrapSelection \{ } begin<CR>", 'v')
	vnoremap <buffer> <silent> <F7> 	:WrapSelection \{ } begin<CR>
    endif
else 
    "Non Default Completion Maps:
    if !hasmapto("<C-R>=atplib#TabCompletion(1)<CR>", 'i')
	imap <silent> <buffer> <F7> 		<C-R>=atplib#TabCompletion(1)<CR>
    endif
    if !hasmapto(" atplib#TabCompletion(1,1)<CR>", 'n')
	nnoremap <silent> <buffer> <F7>		:call atplib#TabCompletion(1,1)<CR>
    endif
    if !hasmapto("<C-R>=atplib#TabCompletion(0)<CR>", 'i')
	imap <silent> <buffer> <S-F7> 		<C-R>=atplib#TabCompletion(0)<CR>
    endif
    if !hasmapto(" atplib#TabCompletion(0,1)<CR>", 'n')
	nnoremap <silent> <buffer> <S-F7>	:call atplib#TabCompletion(0,1)<CR> 
    endif
endif

" Fonts:
if !hasmapto(":WrapSelection {".s:backslash."usefont{".g:atp_font_encoding."}{}{}{}\\selectfont", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."f	:WrapSelection {".s:backslash."usefont{".g:atp_font_encoding."}{}{}{}\\selectfont\\  } ".(len(g:atp_font_encoding)+11)."<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."mbox{", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."mb	:WrapSelection ".s:backslash."mbox{ } begin<CR>"
endif

if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."textrm{'],['".s:backslash."text{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."te	:<C-U>InteligentWrapSelection ['".s:backslash."textrm{'],['".s:backslash."text{']<CR>"
endif
if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."textrm{'],['".s:backslash."mathrm{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."rm	:<C-U>InteligentWrapSelection ['".s:backslash."textrm{'],['".s:backslash."mathrm{']<CR>"
endif
if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."emph{'],['".s:backslash."mathit{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."em	:<C-U>InteligentWrapSelection ['".s:backslash."emph{'],['".s:backslash."mathit{']<CR>"
endif
"   Suggested Maps:
"     execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."tx	:<C-U>InteligentWrapSelection [''],['".s:backslash."text{']<CR>"
"     execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."in	:<C-U>InteligentWrapSelection [''],['".s:backslash."intertext{']<CR>"
if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."textit{'],['".s:backslash."mathit{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."it	:<C-U>InteligentWrapSelection ['".s:backslash."textit{'],['".s:backslash."mathit{']<CR>"
endif
if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."textsf{'],['".s:backslash."mathsf{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."sf	:<C-U>InteligentWrapSelection ['".s:backslash."textsf{'],['".s:backslash."mathsf{']<CR>"
endif
if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."texttt{'],['".s:backslash."mathtt{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."tt	:<C-U>InteligentWrapSelection ['".s:backslash."texttt{'],['".s:backslash."mathtt{']<CR>"
endif
if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."textbf{'],['".s:backslash."mathbf{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."bf	:<C-U>InteligentWrapSelection ['".s:backslash."textbf{'],['".s:backslash."mathbf{']<CR>"
endif
if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."textbf{'],['".s:backslash."mathbb{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."bb	:<C-U>InteligentWrapSelection ['".s:backslash."textbf{'],['".s:backslash."mathbb{']<CR>"
endif
if !hasmapto(":<C-U>WrapSelection ".s:backslash."textsl{<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."sl	:<C-U>WrapSelection ".s:backslash."textsl{<CR>"
endif
if !hasmapto(":<C-U>WrapSelection ".s:backslash."textsc{<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."sc	:<C-U>WrapSelection ".s:backslash."textsc{<CR>"
endif
if !hasmapto(":<C-U>WrapSelection ".s:backslash."textup{<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."up	:<C-U>WrapSelection ".s:backslash."textup{<CR>"
endif
if !hasmapto(":<C-U>WrapSelection ".s:backslash."textmd{<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."md	:<C-U>WrapSelection ".s:backslash."textmd{<CR>"
endif
if !hasmapto(":<C-U>WrapSelection ".s:backslash."underline{<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."un	:<C-U>WrapSelection ".s:backslash."underline{<CR>"
endif
if !hasmapto(":<C-U>WrapSelection ".s:backslash."overline{<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."ov	:<C-U>WrapSelection ".s:backslash."overline{<CR>"
endif
if !hasmapto(":<C-U>InteligentWrapSelection ['".s:backslash."textnormal{'],['".s:backslash."mathnormal{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."no	:<C-U>InteligentWrapSelection ['".s:backslash."textnormal{'],['".s:backslash."mathnormal{']<CR>"
endif
if !hasmapto(":<C-U>InteligentWrapSelection [''],['".s:backslash."mathcal{']<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_text_font_leader."cal	:<C-U>InteligentWrapSelection [''],['".s:backslash."mathcal{']<CR>"
endif

" Environments:
if !hasmapto(":WrapSelection ".s:backslash."begin{center} ".s:backslash."end{center} 0 1<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_environment_leader."C   :WrapSelection ".s:backslash."begin{center} ".s:backslash."end{center} 0 1<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."begin{flushright} ".s:backslash."end{flushright} 0 1<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_environment_leader."R   :WrapSelection ".s:backslash."begin{flushright} ".s:backslash."end{flushright} 0 1<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."begin{flushleft} ".s:backslash."end{flushleft} 0 1<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_environment_leader."L   :WrapSelection ".s:backslash."begin{flushleft} ".s:backslash."end{flushleft} 0 1<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."begin{equation=b:atp_StarMathEnvDefault<CR>} ".s:backslash."end{equation=b:atp_StarMathEnvDefault<CR>} 0 1<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_environment_leader."E   :WrapSelection ".s:backslash."begin{equation=b:atp_StarMathEnvDefault<CR>} ".s:backslash."end{equation=b:atp_StarMathEnvDefault<CR>} 0 1<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."begin{align=b:atp_StarMathEnvDefault<CR>} ".s:backslash."end{align=b:atp_StarMathEnvDefault<CR>} 0 1<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_environment_leader."A   :WrapSelection ".s:backslash."begin{align=b:atp_StarMathEnvDefault<CR>} ".s:backslash."end{align=b:atp_StarMathEnvDefault<CR>} 0 1<CR>"
endif

" Math Modes:
if !hasmapto(':<C-U>WrapSelection '.s:backslash.'( '.s:backslash.')<CR>', 'v')
    exe "vmap <silent> <buffer> m				:<C-U>WrapSelection ".s:backslash."( ".s:backslash.")<CR>"
endif
if !hasmapto(':<C-U>WrapSelection '.s:backslash.'[ '.s:backslash.']<CR>', 'v')
    exe "vmap <silent> <buffer> M				:<C-U>WrapSelection ".s:backslash."[ ".s:backslash."]<CR>"
endif

    " Brackets:
if !hasmapto(":WrapSelection ( ) ", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader."( 	:WrapSelection ( ) begin<CR>"
endif
if !hasmapto(":WrapSelection [ ] ", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader."[ 	:WrapSelection [ ] begin<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."{ ".s:backslash."} begin<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader.s:backslash."{	:WrapSelection ".s:backslash."{ ".s:backslash."} begin<CR>"
endif
if !hasmapto(":WrapSelection { } ", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader."{ 	:WrapSelection { } begin<CR>"
endif
if !hasmapto(":WrapSelection < > ", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader."< 	:WrapSelection < > begin<CR>"
endif
if !hasmapto(":WrapSelection ( ) ", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader.")	:WrapSelection ( ) end<CR>"
endif
if !hasmapto(":WrapSelection [ ] ", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader."]	:WrapSelection [ ] end<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."{ ".s:backslash."}", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader.s:backslash."} :WrapSelection ".s:backslash."{ ".s:backslash."} end<CR>"
endif
if !hasmapto(":WrapSelection { } ", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader."}	:WrapSelection { } end<CR>"
endif
if !hasmapto(":WrapSelection < > ", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_bracket_leader."> 	:WrapSelection < > end<CR>"
endif

if !hasmapto(":WrapSelection ".s:backslash."left( ".s:backslash."right) begin<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_big_bracket_leader."(	:WrapSelection ".s:backslash."left( ".s:backslash."right) begin<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."left[ ".s:backslash."right] begin<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_big_bracket_leader."[	:WrapSelection ".s:backslash."left[ ".s:backslash."right] begin<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."left".s:backslash."{ ".s:backslash."right".s:backslash."} begin<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_big_bracket_leader."{	:WrapSelection ".s:backslash."left".s:backslash."{ ".s:backslash."right".s:backslash."} begin<CR>"
endif
" for compatibility:
if !hasmapto(":WrapSelection ".s:backslash."left".s:backslash."{ ".s:backslash."right".s:backslash."} begin<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_big_bracket_leader.s:backslash."{	:WrapSelection ".s:backslash."left".s:backslash."{ ".s:backslash."right".s:backslash."} begin<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."left( ".s:backslash."right) end<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_big_bracket_leader.")	:WrapSelection ".s:backslash."left( ".s:backslash."right) end<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."left[ ".s:backslash."right] end<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_big_bracket_leader."]	:WrapSelection ".s:backslash."left[ ".s:backslash."right] end<CR>"
endif
if !hasmapto(":WrapSelection ".s:backslash."left".s:backslash."{ ".s:backslash."right".s:backslash."} end<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_big_bracket_leader."}	:WrapSelection ".s:backslash."left".s:backslash."{ ".s:backslash."right".s:backslash."} end<CR>"
endif
" for compatibility:
if !hasmapto(":WrapSelection ".s:backslash."left".s:backslash."{ ".s:backslash."right".s:backslash."} end<CR>", 'v')
    execute "vnoremap <silent> <buffer> ".g:atp_vmap_big_bracket_leader.s:backslash."	:WrapSelection ".s:backslash."left".s:backslash."{ ".s:backslash."right".s:backslash."} end<CR>"
endif

" Tex Align:
if !hasmapto(":TexAlign<CR>", 'n')
    nmap <silent> <buffer> <Localleader>a	:TexAlign<CR>
endif

" Paragraph Selection:
if !hasmapto("<Plug>ATP_SelectCurrentParagraphInner", 'v')
    vmap <silent> <buffer> ip 	<Plug>ATP_SelectCurrentParagraphInner
endif
if !hasmapto("<Plug>ATP_SelectCurrentParagraphOuter", 'v')
    vmap <silent> <buffer> ap 	<Plug>ATP_SelectCurrentParagraphOuter
endif
if !hasmapto(" vip<CR>", "o")
    omap <silent> <buffer>  ip	:normal vip<CR>
endif
if !hasmapto(" vap<CR>", "o")
    omap <silent> <buffer>  ap	:normal vap<CR>
endif

" Formating:
if !hasmapto("m`vipgq``", "n")
    nmap <silent> <buffer> gw		m`vipgq``
endif

" Indent Block:
nnoremap g>	:<C-U>call feedkeys("m`vip".(v:count1 <= 1 ? "" : v:count1).">``", 't')<CR>
nnoremap g<	:<C-U>call feedkeys("m`vip".(v:count1 <= 1 ? "" : v:count1)."<``", 't')<CR>

" Select Syntax:
if !hasmapto("<Plug>SelectOuterSyntax", "v")
    vmap <buffer> <silent> aS		<Plug>SelectOuterSyntax
endif
if !hasmapto("<Plug>SelectInnerSyntax", "v")
    vmap <buffer> <silent> iS		<Plug>SelectInnerSyntax
endif

" Environment Moves:
" From vim.vim plugin (by Bram Mooleaner)
" Move around functions.
exe "nnoremap <silent> <buffer> <Plug>BegPrevEnvironment m':call search('".s:bbackslash."begin".s:backslash."s*{".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash.s:bbackslash."[".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash."$".s:backslash."$', 'bW')<CR>"
if !hasmapto("<Plug>BegPrevEnvironment", "n")
    nmap <silent> <buffer> [[ <Plug>BegPrevEnvironment
endif
exe "vnoremap <silent> <buffer> <Plug>vBegPrevEnvironment m':<C-U>exe \"normal! gv\"<Bar>call search('".s:bbackslash."begin".s:backslash."s*{".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash.s:bbackslash."[".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash."$".s:backslash."$', 'bW')<CR>"
if !hasmapto("<Plug>vBegPrevEnvironment", "v")
    vmap <silent> <buffer> [[ <Plug>vBegPrevEnvironment
endif
exe "nnoremap <silent> <buffer> <Plug>BegNextEnvironment m':call search('".s:bbackslash."begin".s:backslash."s*{".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash.s:bbackslash."[".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash."$".s:backslash."$', 'W')<CR>"
if !hasmapto("<Plug>BegNextEnvironment", "n")
    nmap <silent> <buffer> ]] <Plug>BegNextEnvironment
endif
exe "vnoremap <silent> <buffer> <Plug>vBegNextEnvironment m':<C-U>exe \"normal! gv\"<Bar>call search('".s:bbackslash."begin".s:backslash."s*{".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash.s:bbackslash."[".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash."$".s:backslash."$', 'W')<CR>"
if !hasmapto("<Plug>vBegNextEnvironment", "v")
    vmap <silent> <buffer> ]] <Plug>vBegNextEnvironment
endif
exe "nnoremap <silent> <buffer> <Plug>EndPrevEnvironment m':call search('".s:bbackslash."end".s:backslash."s*{".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash.s:bbackslash."]".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash."$".s:backslash."$', 'bW')<CR>"
if !hasmapto("<Plug>EndPrevEnvironment", "n")
    nmap <silent> <buffer> [] <Plug>EndPrevEnvironment
endif
exe "vnoremap <silent> <buffer> <Plug>vEndPrevEnvironment m':<C-U>exe \"normal! gv\"<Bar>call search('".s:bbackslash."end".s:backslash."s*{".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash.s:bbackslash."]".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash."$".s:backslash."$', 'bW')<CR>"
if !hasmapto("<Plug>vEndPrevEnvironment", "v")
    vmap <silent> <buffer> [] <Plug>vEndPrevEnvironment
endif
exe "nnoremap <silent> <buffer> <Plug>EndNextEnvironment m':call search('".s:bbackslash."end".s:backslash."s*{".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash.s:bbackslash."]".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash."$".s:backslash."$', 'W')<CR>"
if !hasmapto("<Plug>EndNextEnvironment", "n")
    nmap <silent> <buffer> ][ <Plug>EndNextEnvironment
endif
exe "vnoremap <silent> <buffer> <Plug>vEndNextEnvironment m':<C-U>exe \"normal! gv\"<Bar>call search('".s:bbackslash."end".s:backslash."s*{".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash.s:bbackslash."]".s:bbackslash."|".s:backslash.s:bbackslash."@<!".s:backslash."$".s:backslash."$', 'W')<CR>"
if !hasmapto("<Plug>vEndNextEnvironment", "v")
    vmap <silent> <buffer> ][ <Plug>vEndNextEnvironment
endif

" Move Around Comments:
exe "nnoremap <silent> <buffer> <Plug>BegNextComment :call search('^".s:backslash."(".s:backslash."s*%.*".s:backslash."n".s:backslash.")".s:backslash."@<!".s:backslash."(".s:backslash."s*%".s:backslash.")', 'W')<CR>"
if !hasmapto("<Plug>BegNextComment", "n")
    nmap <silent> <buffer> ]% <Plug>BegNextComment
endif
exe "vnoremap <silent> <buffer> <Plug>vBegNextComment :<C-U>exe \"normal! gv\"<Bar>call search('^".s:backslash."(".s:backslash."s*%.*".s:backslash."n".s:backslash.")".s:backslash."@<!".s:backslash."(".s:backslash."s*%".s:backslash.")', 'W')<CR>"
if !hasmapto("<Plug>vBegNextComment", "v")
    vmap <silent> <buffer> ]% <Plug>vBegNextComment
endif
exe "nnoremap <silent> <buffer> <Plug>EndPrevComment 0<Bar>:call search('".s:backslash."%(^".s:backslash."s*%.*".s:backslash."n".s:backslash.")".s:backslash."%(^".s:backslash."s*%".s:backslash.")".s:backslash."@!', 'bW')<CR>"
if !hasmapto("<Plug>EndPrevComment", "n")
    nmap <silent> <buffer> [% <Plug>EndPrevComment
endif
exe "vnoremap <silent> <buffer> <Plug>vEndPrevComment :<C-U>exe \"normal! gv0\"<Bar>call search('".s:backslash."%(^".s:backslash."s*%.*".s:backslash."n".s:backslash.")".s:backslash."%(^".s:backslash."s*%".s:backslash.")".s:backslash."@!', 'bW')<CR>"
if !hasmapto("<Plug>vEndPrevComment", "v")
    vmap <silent> <buffer> [% <Plug>vEndPrevComment
endif

" Select Comment:
if !hasmapto("v<Plug>vSelectComment", "n")
    exe "nmap <silent> <buffer> ".g:atp_MapSelectComment." v<Plug>vSelectComment"
endif

" Normal Mode Maps: (most of them)
if mapcheck('<LocalLeader>v') == "" && !hasmapto("<Plug>ATP_ViewOutput", "n")
    nmap  <silent> <buffer> <LocalLeader>v		<Plug>ATP_ViewOutput
endif

exe "nmap  <silent> <buffer> <Plug>QForwardSearch 	q".s:backslash.":call ATP_CmdwinToggleSpace(1)<CR>i"
if !hasmapto("<Plug>QForwardSearch", "n")
"     nmap  <silent> <buffer> <F2> 			<Plug>QForwardSearch
    if mapcheck('Q/$', 'n') == ""
	nmap <silent> <buffer> Q/			<Plug>QForwardSearch
    endif
endif
exe "nmap <silent> <buffer> <Plug>QBackwardSearch	q?:call ATP_CmdwinToggleSpace(1)<CR>"
if mapcheck('Q?$', 'n') == "" && !hasmapto("<Plug>QBackwardSearch", "n")
    nmap <silent> <buffer> Q?				<Plug>QBackwardSearch
endif

if mapcheck('<LocalLeader>s$') == "" && !hasmapto("<Plug>ToggleStar", "n")
    nmap  <silent> <buffer> <LocalLeader>s		<Plug>ToggleStar
elseif !hasmapto("<Plug>ToggleStar", "n") && g:atp_debugMapFile && !g:atp_reload_functions
    echoerr "[ATP:] there will be no nmap to <Plug>ToggleStar"
endif

if !hasmapto("<Plug>TogglesilentMode", "n")
    nmap  <silent> <buffer> <LocalLeader><Localleader>s	<Plug>TogglesilentMode
endif
if !hasmapto("<Plug>ToggledebugMode", "n")
    nmap  <silent> <buffer> <LocalLeader><Localleader>d	<Plug>ToggledebugMode
endif
if !hasmapto("<Plug>ToggleDebugMode", "n")
    nmap  <silent> <buffer> <LocalLeader><Localleader>D	<Plug>ToggleDebugMode
endif
if !hasmapto("<Plug>WrapEnvironment", "v")
    vmap  <silent> <buffer> <F4>			<Plug>WrapEnvironment
endif
if !hasmapto("<Plug>ChangeEnv", "n")
    nmap  <silent> <buffer> <F4>			<Plug>ChangeEnv
endif
if !hasmapto("<Plug>ChangeEnv", "i")
    imap  <silent> <buffer> <F4>			<C-O><Plug>ChangeEnv
endif
if !hasmapto("<Plug>ToggleEnvForward", "n")
    nmap  <silent> <buffer> <S-F4>			<Plug>ToggleEnvForward
endif
"     nmap  <silent> <buffer> <S-F4>			<Plug>ToggleEnvBackward
if !hasmapto("<Plug>LatexEnvPrompt", "n")
    nmap  <silent> <buffer> <C-S-F4>			<Plug>LatexEnvPrompt
endif
"     ToDo:
"     if g:atp_LatexBox
" 	nmap <silent> <buffer> <F3>			:call <Sid>ChangeEnv()<CR>
"     endif
if !hasmapto("<Plug>ATP_ViewOutput", "n")
    nmap  <silent> <buffer> <F3>        		<Plug>ATP_ViewOutput
endif
if !hasmapto("<Plug>ATP_ViewOutput", "i")
    imap  <silent> <buffer> <F3> 			<C-O><Plug>ATP_ViewOutput
endif
if !hasmapto("<Plug>Getpid", "n")
    nmap  <silent> <buffer> <LocalLeader>g 		<Plug>Getpid
endif
if !hasmapto("<Plug>ATP_TOC", "n")
    nmap  <silent> <buffer> <LocalLeader>t		<Plug>ATP_TOC
endif
if !hasmapto("<Plug>ATP_Labels", "n")
    nmap  <silent> <buffer> <LocalLeader>L		<Plug>ATP_Labels
endif
if !hasmapto("<Plug>ATP_TeXCurrent", "n")
    nmap  <silent> <buffer> <LocalLeader>l 		<Plug>ATP_TeXCurrent
endif
if !hasmapto("<Plug>ATP_TeXdebug", "n")
    nmap  <silent> <buffer> <LocalLeader>d 		<Plug>ATP_TeXdebug
endif
if !hasmapto("<Plug>ATP_TeXDebug", "n")
    nmap  <silent> <buffer> <LocalLeader>D 		<Plug>ATP_TeXDebug
endif
" if !hasmapto("<Plug>ATP_MakeLatex", "n")
"      nmap           <buffer> <c-l>			<Plug>ATP_MakeLatex
" endif
"ToDo: imaps!
if !hasmapto("<Plug>ATP_TeXVerbose", "n")
    nmap  <silent> <buffer> <F5> 			<Plug>ATP_TeXVerbose
endif
if !hasmapto("<Plug>ToggleAuTeX", "n")
    nmap  <silent> <buffer> <s-F5> 			<Plug>ToggleAuTeX
endif
if !hasmapto("<Plug>ToggleAuTeXa", "i")
    imap  <silent> <buffer> <s-F5> 			<C-O><Plug>ToggleAuTeX
endif
if !hasmapto("<Plug>ToggleTab", "n")
    nmap  <silent> <buffer> `<Tab>			<Plug>ToggleTab
endif
if !hasmapto("<Plug>ToggleTab", "i")
    imap  <silent> <buffer> `<Tab>			<Plug>ToggleTab
endif
if !hasmapto("<Plug>ToggleIMaps", "n")
    nmap  <silent> <buffer> '<Tab>			<Plug>ToggleIMaps
endif
if !hasmapto("<Plug>ToggleIMapsa", "i")
    imap  <silent> <buffer> '<Tab>			<Plug>ToggleIMapsa
endif
if !hasmapto("<Plug>SimpleBibtex", "n")
    nmap  <silent> <buffer> <LocalLeader>B		<Plug>SimpleBibtex
endif
if !hasmapto("<Plug>BibtexDefault", "n")
    nmap  <silent> <buffer> <LocalLeader>b		<Plug>BibtexDefault
endif
if !hasmapto("<Plug>Delete", "n")
    nmap  <silent> <buffer> <F6>d 			<Plug>Delete
endif
if !hasmapto("<Plug>Delete", "i")
    imap  <silent> <buffer> <F6>d			<C-O><Plug>Delete
endif
if !hasmapto("<Plug>OpenLog", "n")
    nmap  <silent> <buffer> <F6>l 			<Plug>OpenLog
endif
if !hasmapto("<Plug>OpenLog", "i")
    imap  <silent> <buffer> <F6>l 			<C-O><Plug>OpenLog
endif
if !hasmapto(":ShowErrors e<CR>", "n")
    nnoremap  <silent> <buffer> <F6> 			:ShowErrors e<CR>
endif
if !hasmapto(":ShowErrors e<CR>", "i")
    inoremap  <silent> <buffer> <F6> 			:ShowErrors e<CR>
endif
if !hasmapto(":ShowErrors<CR>", "n")
    noremap   <silent> <buffer> <LocalLeader>e		:ShowErrors<CR>
endif
if !hasmapto(":ShowErrors w<CR>", "n")
    nnoremap  <silent> <buffer> <F6>w 			:ShowErrors w<CR>
endif
if !hasmapto(":ShowErrors w<CR>", "i")
    inoremap  <silent> <buffer> <F6>w 			:ShowErrors w<CR>
endif
if !hasmapto(":ShowErrors rc<CR>", "n")
    nnoremap  <silent> <buffer> <F6>r 			:ShowErrors rc<CR>
endif
if !hasmapto(":ShowErrors rc<CR>", "i")
    inoremap  <silent> <buffer> <F6>r 			:ShowErrors rc<CR>
endif
if !hasmapto(":ShowErrors f<CR>", "n")
    nnoremap  <silent> <buffer> <F6>f 			:ShowErrors f<CR>
endif
if !hasmapto(":ShowErrors f<CR>", "i")
    inoremap  <silent> <buffer> <F6>f 			:ShowErrors f<CR>
endif
if !hasmapto("<Plug>PdfFonts", "n")
    nnoremap  <silent> <buffer> <F6>g 			<Plug>PdfFonts
endif

" TeXdoc:
" Note :TexDoc map cannot be <silent>
nnoremap           <buffer> <Plug>TexDoc		:TexDoc<space>
if !hasmapto("<Plug>TexDoc", "n")
    nmap           <buffer> <F1>			<Plug>TexDoc
endif
inoremap           <buffer> <Plug>iTexDoc		<C-O>:TexDoc<space>
if !hasmapto("<Plug>iTexDoc", "i")
    imap           <buffer> <F1> 			<Plug>iTexDoc
endif

" Font Maps:
if g:atp_imap_first_leader == "]" || g:atp_imap_second_leader == "]" || g:atp_imap_third_leader == "]" || g:atp_imap_fourth_leader == "]" 
    inoremap <silent> <buffer> ]] ]
endif
if !exists("g:atp_imap_define_fonts")
    let g:atp_imap_define_fonts = 1
endif
if !exists("g:atp_imap_fonts") || g:atp_reload_variables
let g:atp_imap_fonts = [
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'rm', 
	\ '<Esc>:call Insert("'.s:bbackslash.'textrm{}", "'.s:bbackslash.'mathrm{}", 1)<CR>a', "g:atp_imap_define_fonts", 'rm font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'up', 
	\ s:backslash.'textup{}<Left>', "g:atp_imap_define_fonts", 'up font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'md', 
	\ s:backslash.'textmd{}<Left>', "g:atp_imap_define_fonts", 'md font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'it', 
	\ '<Esc>:call Insert("'.s:bbackslash.'textit{}", "'.s:bbackslash.'mathit{}", 1)<CR>a', "g:atp_imap_define_fonts", 'it font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'sl', 
	\ s:backslash.'textsl{}<Left>', "g:atp_imap_define_fonts", 'sl font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'sc', 
	\ s:backslash.'textsc{}<Left>', "g:atp_imap_define_fonts", 'sc font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'sf', 
	\ '<Esc>:call Insert("'.s:bbackslash.'textsf{}", "'.s:bbackslash.'mathsf{}", 1)<CR>a', "g:atp_imap_define_fonts", 'sf font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'bf', 
	\ '<Esc>:call Insert("'.s:bbackslash.'textbf{}", "'.s:bbackslash.'mathbf{}", 1)<CR>a', "g:atp_imap_define_fonts", 'bf font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'tt', 
	\ '<Esc>:call Insert("'.s:bbackslash.'texttt{}", "'.s:bbackslash.'mathtt{}", 1)<CR>a', "g:atp_imap_define_fonts", 'tt font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'em', 
	\ s:backslash.'emph{}<Left>', "g:atp_imap_define_fonts", 'emphasize font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'no', 
	\ '<Esc>:call Insert("'.s:bbackslash.'textnormal{}", "'.s:bbackslash.'mathnormal{}", 1)<Cr>a', "g:atp_imap_define_fonts", 'normal font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'bb', 
	\ s:backslash.'mathbb{}<Left>', "g:atp_imap_define_fonts", 'mathbb font'],
    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_second_leader, 'cal', 
	\ s:backslash.'mathcal{}<Left>', "g:atp_imap_define_fonts", 'mathcal font'],
\ ]
endif
    " Make Font Maps:
    call atplib#MakeMaps(g:atp_imap_fonts)
	    
" Greek Letters:
if !exists("g:atp_imap_greek_letters") || g:atp_reload_variables
    let g:atp_imap_greek_letters= [
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'a', s:backslash.'alpha',	 
		    \ "g:atp_imap_define_greek_letters", '\alpha' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'b', s:backslash.'beta',	 
		    \ "g:atp_imap_define_greek_letters", '\beta' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'c', s:backslash.'chi',	 
		    \ "g:atp_imap_define_greek_letters", '\chi' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'd', s:backslash.'delta',	 
		    \ "g:atp_imap_define_greek_letters", '\delta' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'e', s:backslash.'epsilon',	 
		    \ "g:atp_imap_define_greek_letters", '\epsilon' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'v', s:backslash.'varepsilon', 
		    \ "g:atp_imap_define_greek_letters", '\varepsilon' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'f', s:backslash.'phi',	 
		    \ "g:atp_imap_define_greek_letters", '\phi' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'y', s:backslash.'psi',	 
		    \ "g:atp_imap_define_greek_letters", '\psi' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'g', s:backslash.'gamma',	 
		    \ "g:atp_imap_define_greek_letters", '\gamma' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'h', s:backslash.'eta',	 
		    \ "g:atp_imap_define_greek_letters", '\eta' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'k', s:backslash.'kappa',	 
		    \ "g:atp_imap_define_greek_letters", '\kappa' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'l', s:backslash.'lambda',	 
		    \ "g:atp_imap_define_greek_letters", '\lambda' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'i', s:backslash.'iota',	 
		    \ "g:atp_imap_define_greek_letters", '\iota' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'm', s:backslash.'mu',	 
		    \ "g:atp_imap_define_greek_letters", '\mu' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'n', s:backslash.'nu',	 
		    \ "g:atp_imap_define_greek_letters", '\nu' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'p', s:backslash.'pi',	 
		    \ "g:atp_imap_define_greek_letters", '\pi' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'o', s:backslash.'theta',	 
		    \ "g:atp_imap_define_greek_letters", '\theta' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'r', s:backslash.'rho',	 
		    \ "g:atp_imap_define_greek_letters", '\rho' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 's', s:backslash.'sigma',	 
		    \ "g:atp_imap_define_greek_letters", '\sigma' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 't', s:backslash.'tau',	 
		    \ "g:atp_imap_define_greek_letters", '\tau' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'u', s:backslash.'upsilon',	 
		    \ "g:atp_imap_define_greek_letters", '\upsilon' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'v', s:backslash.'varsigma',	 
		    \ "g:atp_imap_define_greek_letters", '\varsigma' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'v', s:backslash.'vartheta',	 
		    \ "g:atp_imap_define_greek_letters", '\vartheta' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'w', s:backslash.'omega',	 
		    \ "g:atp_imap_define_greek_letters", '\omega' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'x', s:backslash.'xi',	 
		    \ "g:atp_imap_define_greek_letters", '\xi' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'z', s:backslash.'zeta',	 
		    \ "g:atp_imap_define_greek_letters", '\zeta' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'D', s:backslash.'Delta',	 
		    \ "g:atp_imap_define_greek_letters", '\Delta' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'Y', s:backslash.'Psi',	 
		    \ "g:atp_imap_define_greek_letters", '\Psi' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'F', s:backslash.'Phi',	 
		    \ "g:atp_imap_define_greek_letters", '\Phi' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'G', s:backslash.'Gamma',	 
		    \ "g:atp_imap_define_greek_letters", '\Gamma' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'L', s:backslash.'Lambda',	 
		    \ "g:atp_imap_define_greek_letters", '\Lambda' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'M', s:backslash.'Mu',	 
		    \ "g:atp_imap_define_greek_letters", '\Mu' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'P', s:backslash.'Pi',	 
		    \ "g:atp_imap_define_greek_letters", '\Pi' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'O', s:backslash.'Theta',	 
		    \ "g:atp_imap_define_greek_letters", '\Theta' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'S', s:backslash.'Sigma',	 
		    \ "g:atp_imap_define_greek_letters", '\Sigma' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'T', s:backslash.'Tau',	 
		    \ "g:atp_imap_define_greek_letters", '\Tau' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'U', s:backslash.'Upsilon',	 
		    \ "g:atp_imap_define_greek_letters", '\Upsilon' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'W', s:backslash.'Omega',	 
		    \ "g:atp_imap_define_greek_letters", '\Omega' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'Z', s:backslash.'mathrm',	 
		    \ "g:atp_imap_define_greek_letters", '\mathrm' ],
	    \ ]
endif

    " Make Greek Letters:
    augroup ATP_MathIMaps_GreekLetters
	au!
	au CursorMovedI	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_greek_letters, 'CursorMovedI')
	au CursorHoldI 	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_greek_letters, 'CursorHoldI')
	au InsertEnter	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_greek_letters, 'InsertEnter') 
	" Make imaps visible with :imap /this will not work with i_CTRL-C/
	au InsertLeave	*.tex 	:call atplib#MakeMaps(g:atp_imap_greek_letters, 'InsertLeave')
	au BufEnter	*.tex 	:call atplib#MakeMaps(g:atp_imap_greek_letters, 'BufEnter')
    augroup END

" Miscellaneous Mathematical Maps:
if !exists("g:atp_imap_math_misc") || g:atp_reload_variables
    if !exists("g:atp_infty_leader")
	let g:atp_infty_leader = (g:atp_imap_first_leader == '#' ? '`' : g:atp_imap_first_leader ) 
    endif
let g:atp_imap_math_misc = [
\ [ 'inoremap', '<silent> <buffer>', '+',		      '+', s:backslash.'sum',
	\ "g:atp_imap_define_math_misc", '\sum' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_infty_leader,      '8', s:backslash.'infty', 	
	\ "g:atp_imap_define_math_misc", '\infty' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_infty_leader,      '6', s:backslash.'partial',	
	\ "g:atp_imap_define_math_misc", '\partial' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '&', s:backslash.'wedge', 	
	\ "g:atp_imap_define_math_misc", '\wedge' ], 
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 've', s:backslash.'vee', 	
	\ "g:atp_imap_define_math_misc", '\vee' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'V', s:backslash.'bigvee', 	
	\ "g:atp_imap_define_math_misc", '\Vee' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '+', s:backslash.'bigcup', 	
	\ "g:atp_imap_define_math_misc", '\bigcup' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '*', s:backslash.'bigcap', 	
	\ "g:atp_imap_define_math_misc", '\bigcap' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, s:backslash, s:backslash.'s:backslash', 
	\ "g:atp_imap_define_math_misc", '\s:backslash' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, 'N', s:backslash.'Nabla', 	
	\ "g:atp_imap_define_math_misc", '\Nabla' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '@', s:backslash.'circ', 	
	\ "g:atp_imap_define_math_misc", '\circ' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '=', s:backslash.'equiv', 	
	\ "g:atp_imap_define_math_misc", '\equiv' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '>', s:backslash.'geq', 	
	\ "g:atp_imap_define_math_misc", '\geq' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '<', s:backslash.'leq', 	
	\ "g:atp_imap_define_math_misc", '\leq' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '.', s:backslash.'dot', 	
	\ "g:atp_imap_define_math_misc", '\dot' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '/', s:backslash.'frac{}{}<Esc>F}i', 	
	\ "g:atp_imap_define_math_misc", '\frac{}{}' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '`', s:backslash.'grave{}<Left>', 	
	\ "g:atp_imap_define_math_misc", '\grave{}' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  'v', s:backslash.'check{}<Left>', 	
	\ "g:atp_imap_define_math_misc", '\check{}' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '''', s:backslash.'acute{}<Left>', 	
	\ "g:atp_imap_define_math_misc", '\acute{}' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '.', s:backslash.'dot{}<Left>', 	
	\ "g:atp_imap_define_math_misc", '\dot{}' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '>', s:backslash.'vec{}<Left>', 	
	\ "g:atp_imap_define_math_misc", '\vec{}' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '_', s:backslash.'bar{}<Left>', 	
	\ "g:atp_imap_define_math_misc", '\bar{}' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '~', s:backslash.'=(g:atp_imap_wide ? "wide" : "")<CR>tilde{}<Left>', 	
	\ "g:atp_imap_define_math_misc", '''\''.(g:atp_imap_wide ? "wide" : "")."tilde"' ],
\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '^', s:backslash.'=(g:atp_imap_wide ? "wide" : "" )<CR>hat{}<Left>', 	
	\ "g:atp_imap_define_math_misc", '''\''.(g:atp_imap_wide ? "wide" : "")."hat"' ], 
\ ]
" 		\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '~', s:backslash.'=(g:atp_imap_wide ? "wide" : "")<CR>tilde{}<Left>', 	"g:atp_imap_define_math_misc", '''\''.(g:atp_imap_wide ? "wide" : "")."tilde"' ],
" 		\ [ 'inoremap', '<silent> <buffer>', g:atp_imap_first_leader, '^', s:backslash.'=(g:atp_imap_wide ? "wide" : "" )<CR>hat{}<Left>', 	"g:atp_imap_define_math_misc", '''\''.(g:atp_imap_wide ? "wide" : "")."hat"' ], 
endif

    " Make Miscellaneous Mathematical Maps:
    augroup ATP_MathIMaps_misc
	au!
	au CursorMovedI	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_math_misc, 'CursorMovedI', g:atp_imap_diacritics)
	au CursorHoldI 	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_math_misc, 'CursorHoldI', g:atp_imap_diacritics) 
	au InsertEnter	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_math_misc, 'InsertEnter', g:atp_imap_diacritics) 
	" Make imaps visible with :imap /this will not work with i_CTRL-C/
" 	au InsertLeave	*.tex 	:call atplib#MakeMaps(g:atp_imap_math_misc, 'InsertLeave')
" 	au BufEnter	*.tex 	:call atplib#MakeMaps(g:atp_imap_math_misc, 'BufEnter')
    augroup END

    let g:atp_imap_diacritics = [
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '''', s:backslash.'''{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\''{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '"', s:backslash.'"{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\"{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '^', s:backslash.'^{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\^{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  'v', s:backslash.'v{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\v{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  'b', s:backslash.'b{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\b{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  'd', s:backslash.'d{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\d{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '`', s:backslash.'`{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\`{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  'H', s:backslash.'H{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\H{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '~', s:backslash.'~{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\~{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  '.', s:backslash.'.{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\.{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  'c', s:backslash.'c{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\c{}' ],
	    \ [ 'inoremap', '<silent> <buffer>', g:atp_imap_over_leader,  't', s:backslash.'t{}<Left>', 	
		    \ "g:atp_imap_define_diacritics", '\t{}' ]
	    \ ]

" Environment Maps:
if g:atp_no_env_maps != 1
    if !exists("g:atp_imap_environments") || g:atp_reload_variables
    let g:atp_imap_environments = [
	\ [ "inoremap", "<buffer> <silent>", 	g:atp_imap_third_leader, "m", 				s:backslash.'('.s:backslash.')<Left><Left>', 						"g:atp_imap_define_environments", 'inlince math' ],
	\ [ "inoremap", "<buffer> <silent>", 	g:atp_imap_third_leader, "M", 				s:backslash.'['.s:backslash.']<Left><Left>', 						"g:atp_imap_define_environments", 'displayed math' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_begin, 		s:backslash.'begin{}<Left>', 						"g:atp_imap_define_environments", '\begin{}' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_end, 		s:backslash.'end{}<Left>', 						"g:atp_imap_define_environments", '\end{}' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_proof, 		s:backslash.'begin{proof}<CR>'.s:backslash.'end{proof}<Esc>O', 				"g:atp_imap_define_environments", 'proof' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_center, 		s:backslash.'begin{center}<CR>\end{center}<Esc>O', 			"g:atp_imap_define_environments", 'center' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_flushleft, 		s:backslash.'begin{flushleft}<CR>'.s:backslash.'end{flushleft}<Esc>O', 			"g:atp_imap_define_environments", 'flushleft' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_flushright, 	s:backslash.'begin{flushright}<CR>'.s:backslash.'end{flushright}<Esc>O', 		"g:atp_imap_define_environments", 'flushright' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_bibliography, 	s:backslash.'begin{thebibliography}<CR>'.s:backslash.'end{thebibliography}<Esc>O', 	"g:atp_imap_define_environments", 'bibliography' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_abstract, 		s:backslash.'begin{abstract}<CR>'.s:backslash.'end{abstract}<Esc>O', 			"g:atp_imap_define_environments", 'abstract' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_item, 		'<Esc>:call InsertItem()<CR>a', 				"g:atp_imap_define_environments", 'item' 	],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_frame, 		s:backslash.'begin{frame}<CR>'.s:backslash.'end{frame}<Esc>O', 				"g:atp_imap_define_environments", 'frame' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_enumerate, 		s:backslash.'begin{enumerate}'.g:atp_EnvOptions_enumerate.'<CR>'.s:backslash.'end{enumerate}<Esc>O'.s:backslash.'item', 	"g:atp_imap_define_environments", 'enumerate' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_itemize, 		s:backslash.'begin{itemize}'.g:atp_EnvOptions_itemize.'<CR>'.s:backslash.'end{itemize}<Esc>O'.s:backslash.'item', 		"g:atp_imap_define_environments", 'itemize' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_tikzpicture, 	s:backslash.'begin{center}<CR>'.s:backslash.'begin{tikzpicture}<CR>'.s:backslash.'end{tikzpicture}<CR>'.s:backslash.'end{center}<Up><Esc>O', "g:atp_imap_define_environments", 'tikzpicture' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_theorem, 		s:backslash.'begin{=g:atp_EnvNameTheorem<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{=g:atp_EnvNameTheorem<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O',  	"g:atp_imap_define_environments", 'theorem'],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_definition, 	s:backslash.'begin{=g:atp_EnvNameDefinition<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{=g:atp_EnvNameDefinition<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O', 	"g:atp_imap_define_environments", 'definition'],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_proposition, 	s:backslash.'begin{=g:atp_EnvNameProposition<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{=g:atp_EnvNameProposition<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O', 	"g:atp_imap_define_environments", 'proposition' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_lemma, 		s:backslash.'begin{=g:atp_EnvNameLemma<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{=g:atp_EnvNameLemma<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O', 		"g:atp_imap_define_environments", 'lemma' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_remark, 		s:backslash.'begin{=g:atp_EnvNameRemark<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{=g:atp_EnvNameRemark<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O', 		"g:atp_imap_define_environments", 'remark' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_note, 		s:backslash.'begin{=g:atp_EnvNameNote<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{=g:atp_EnvNameNote<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O', 		"g:atp_imap_define_environments", 'note' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_example, 		s:backslash.'begin{example=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{example=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O', 		"g:atp_imap_define_environments", 'example' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_corollary, 		s:backslash.'begin{=g:atp_EnvNameCorollary<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{=g:atp_EnvNameCorollary<CR>=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O', 	"g:atp_imap_define_environments", 'corollary' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_align, 		s:backslash.'begin{align=(getline(".")[col(".")-2]=="*"?"":b:atp_StarMathEnvDefault)<CR>}<CR>'.s:backslash.'end{align=(getline(".")[col(".")-2]=="*"?"":b:atp_StarMathEnvDefault)<CR>}<Esc>O', 	"g:atp_imap_define_environments", 'align' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_equation, 		s:backslash.'begin{equation=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<CR>'.s:backslash.'end{equation=(getline(".")[col(".")-2]=="*"?"":b:atp_StarEnvDefault)<CR>}<Esc>O', 	"g:atp_imap_define_environments", 'equation' ],
	\ [ 'inoremap', '<silent> <buffer>',	g:atp_imap_third_leader, g:atp_imap_letter, 		s:backslash.'begin{letter}{}<CR>'.s:backslash.'opening{=g:atp_letter_opening<CR>}<CR>'.s:backslash.'closing{=g:atp_letter_closing<CR>}<CR>'.s:backslash.'end{letter}<Esc>?'.s:bbackslash.'begin{letter}{'.s:backslash.'zs<CR>i', 				"g:atp_imap_define_environments", 'letter' ],
	\ ]
    endif
    " Make Environment Maps:
    call atplib#MakeMaps(g:atp_imap_environments)
endif

" Mathematical Maps:
if !exists("g:atp_imap_math") || g:atp_reload_variables
    let g:atp_imap_math= [ 
	\ [ "inoremap", "<buffer> <silent> <expr>", "", g:atp_imap_subscript, "( g:atp_imap_subscript == '_' && !atplib#IsLeft('\\', 1) && atplib#IsLeft('_') <bar><bar> g:atp_imap_subscript != '_' ) && atplib#IsInMath() ? (g:atp_imap_subscript == '_' ? '<BS>' : '' ).'_{}<Left>' : '_' ", "g:atp_imap_define_math", 	'_{}'], 
	\ [ "inoremap", "<buffer> <silent> <expr>", "", g:atp_imap_supscript, "( g:atp_imap_supscript == '^' && !atplib#IsLeft('\\', 1) && atplib#IsLeft('^') <bar><bar> g:atp_imap_supscript != '^' ) && atplib#IsInMath() ? (g:atp_imap_supscript == '^' ? '<BS>' : '' ).'^{}<Left>' : (atplib#IsLeft('~') ? '<BS>".s:backslash."=(g:atp_imap_wide ? ''wide'' : '''' )<CR>hat{}<Left>' : '^') ", "g:atp_imap_define_math", 	'^{}'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "~", "atplib#IsLeft('~') && atplib#IsInMath() ? '<BS>".s:backslash."=(g:atp_imap_wide ? \"wide\" : \"\" ) <CR>tilde{}<Left>' : '~' " , "g:atp_imap_define_math", 	'^{}'], 
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "=", "atplib#IsInMath() && atplib#IsLeft('=') && !atplib#IsLeft('&',1) ? '<BS>&=' : '='", "g:atp_imap_define_math",	'&=' ],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "o+", "atplib#IsInMath() ? '".s:backslash."oplus' 	: 'o+' ", "g:atp_imap_define_math", 		'\\oplus' ],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "O+", "atplib#IsInMath() ? '".s:backslash."bigoplus' 	: 'O+' ", "g:atp_imap_define_math",		'\\bigoplus'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "o-", "atplib#IsInMath() ? '".s:backslash."ominus' 	: 'o-' ", "g:atp_imap_define_math",		'\\ominus'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "o.", "atplib#IsInMath() ? '".s:backslash."odot' 	: 'o.' ", "g:atp_imap_define_math",			'\\odot'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "O.", "atplib#IsInMath() ? '".s:backslash."bigodot' 	: 'O.' ", "g:atp_imap_define_math",		'\\bigodot'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "o*", "atplib#IsInMath() ? '".s:backslash."otimes' 	: 'o*' ", "g:atp_imap_define_math",		'\\otimes'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "O*", "atplib#IsInMath() ? '".s:backslash."bigotimes' 	: 'O*' ", "g:atp_imap_define_math",		'\\bigotimes'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "s+", "atplib#IsInMath() ? '".s:backslash."cup' 	: 's+' ", "g:atp_imap_define_math",			'\\cup'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "s-", "atplib#IsInMath() ? '".s:backslash."setminus' 	: 's-' ", "g:atp_imap_define_math",		'\\cup'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "S+", "atplib#IsInMath() ? '".s:backslash."bigcup' 	: 'S+' ", "g:atp_imap_define_math",		'\\bigcup'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "s*", "atplib#IsInMath() ? '".s:backslash."cap' 	: 's*' ", "g:atp_imap_define_math",			'\\cap'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "S*", "atplib#IsInMath() ? '".s:backslash."bigcap' 	: 'S*' ", "g:atp_imap_define_math",		'\\bigcap'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "c*", "atplib#IsInMath() ? '".s:backslash."prod' 	: 'c*' ", "g:atp_imap_define_math",			'\\prod'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "c+", "atplib#IsInMath() ? '".s:backslash."coprod' 	: 'c+' ", "g:atp_imap_define_math",		'\\coprod'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "t<", "atplib#IsInMath() ? '".s:backslash."triangleleft' : 't<' ", "g:atp_imap_define_math",		'\\triangleleft'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "t>", "atplib#IsInMath() ? '".s:backslash."triangleright' : 't>' ", "g:atp_imap_define_math",		'\\triangleright'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "s<", "atplib#IsInMath() ? '".s:backslash."subseteq' 	: 's<' ", "g:atp_imap_define_math",		'\\subseteq'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "s>", "atplib#IsInMath() ? '".s:backslash."supseteq' 	: 's>' ", "g:atp_imap_define_math",		'\\supseteq'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", "<=", "atplib#IsInMath() ? '".s:backslash."leq' 	: '<=' ", "g:atp_imap_define_math",			'\\leq'],
	\ [ "inoremap", "<buffer> <silent> <expr>", "", ">=", "atplib#IsInMath() ? '".s:backslash."geq' 	: '>=' ", "g:atp_imap_define_math",			'\\geq'],
	\ ]
endif

    " Make Mathematical Maps:
    augroup ATP_MathIMaps
	au!
	au CursorMovedI	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_math, 'CursorMovedI')
	au CursorHoldI 	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_math, 'CursorHoldI')
	au InsertEnter	*.tex 	:call atplib#ToggleIMaps(g:atp_imap_math, 'InsertEnter')
	" Make imaps visible with :imap  /this will not work with i_CTRL-C/
	au InsertLeave	*.tex 	:call atplib#MakeMaps(g:atp_imap_math, 'InsertLeave')
	au BufEnter	*.tex 	:call atplib#MakeMaps(g:atp_imap_math, 'BufEnter')
    augroup END

" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1:nowrap
