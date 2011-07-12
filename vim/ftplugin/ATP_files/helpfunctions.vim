" Author: 	Marcin Szamotulski
" Description: 	This file contains help commands and variables (for mappings used by ATP) 
" Note:		This file is a part of Automatic Tex Plugin for Vim.
" Language:	tex
" Last Change:

let s:sourced = !exists("s:loaded") ? 0 : 1

if !s:sourced
" {{{1 Help Math IMAPS
function! <SID>HelpMathIMaps()

    if exists("g:no_plugin_maps") || exists("g:no_atp_maps")
	echomsg "[ATP:] ATP maps are turned off"
	return ''
    endif

"     let infty_leader = (g:atp_imap_first_leader == "#" ? "\\" : g:atp_imap_first_leader ) 

    let g:help_mathimaps = ''
	\."\n MATH IMAPS"
	\."\n <maplocalleader> has value g:atp_imap_first_leader"
	\."\n ".g:atp_imap_first_leader."a \\alpha            ".g:atp_imap_first_leader."b \\beta"
	\."\n ".g:atp_imap_first_leader."g \\gamma            ".g:atp_imap_first_leader."d \\delta"
	\."\n ".g:atp_imap_first_leader."e \\epsilon          ".g:atp_imap_first_leader."ve \\varepsilon"
	\."\n ".g:atp_imap_first_leader."z \\zeta             ".g:atp_imap_first_leader."h \\eta"
	\."\n ".g:atp_imap_first_leader."o \\theta            ".g:atp_imap_first_leader."vo \\vartheta"
	\."\n ".g:atp_imap_first_leader."i \\iota             ".g:atp_imap_first_leader."k \\kappa"
	\."\n ".g:atp_imap_first_leader."l \\lambda           ".g:atp_imap_first_leader."m \\mu"
	\."\n ".g:atp_imap_first_leader."n \\nu               ".g:atp_imap_first_leader."x \\xi"
	\."\n ".g:atp_imap_first_leader."p \\pi               ".g:atp_imap_first_leader."r \\rho"
	\."\n ".g:atp_imap_first_leader."s \\sigma            ".g:atp_imap_first_leader."vs \\varsigma" 
	\."\n ".g:atp_imap_first_leader."t \\tau              ".g:atp_imap_first_leader."u \\upsilon"
	\."\n ".g:atp_imap_first_leader."f \\phi              ".g:atp_imap_first_leader."c \\chi"
	\."\n ".g:atp_imap_first_leader."y \\psi              ".g:atp_imap_first_leader."w \\omega"
	\."\n"
	\."\n ".g:atp_imap_first_leader."G \\Gamma            ".g:atp_imap_first_leader."D \\Delta"
	\."\n ".g:atp_imap_first_leader."Z \\mathrm{Z}        ".g:atp_imap_first_leader."O \\Theta"
	\."\n ".g:atp_imap_first_leader."L \\Lambda           ".g:atp_imap_first_leader."M \\Mu"
	\."\n ".g:atp_imap_first_leader."N \\Nu               ".g:atp_imap_first_leader."P \\Pi"
	\."\n ".g:atp_imap_first_leader."S \\Sigma            ".g:atp_imap_first_leader."U \\Upsilon"
	\."\n ".g:atp_imap_first_leader."F \\Phi              ".g:atp_imap_first_leader."Y \\Psi"
	\."\n ".g:atp_imap_first_leader."W \\Omega"
	\."\n"
	\."\n ".g:atp_imap_first_leader."+ \\bigcup           ".g:atp_imap_first_leader."- \\setminus" 
	\."\n ".g:atp_infty_leader."8 \\infty            ".g:atp_imap_first_leader."& \\wedge"
	\."\n ".                        "^^ ^{}               ".                        "__ _{}"
	\."\n ".g:atp_imap_third_leader."m \\(\\)              ".g:atp_imap_third_leader."M \\[\\]           <maplocalleader> has value g:atp_imap_third_leader" 
    return g:help_mathimaps
endfunction
silent call <SID>HelpMathIMaps()

" {{{1 Help Environment IMAPS
function! <SID>HelpEnvIMaps()

    if exists("g:no_plugin_maps") || exists("g:no_atp_maps")
	echomsg "[ATP:] ATP maps are turned off"
	return ''
    endif

    let g:help_envimaps = ''
		\."\n ENVIRONMENT IMAPS" 
		\."\n <maplocalleader> has value g:atp_imap_third_leader"
		\."\n ".(g:atp_imap_begin != "" ? g:atp_imap_third_leader.g:atp_imap_begin." \\begin{}             " : "" ).(g:atp_imap_end != "" ? g:atp_imap_third_leader.g:atp_imap_end." \\end{}" : "")
		\."\n ".(g:atp_imap_theorem != "" ? g:atp_imap_third_leader.g:atp_imap_theorem." theorem              " : "" ).(g:atp_imap_definition != "" ? g:atp_imap_third_leader.g:atp_imap_definition." definition" : "")
		\."\n ".(g:atp_imap_proposition != "" ? g:atp_imap_third_leader.g:atp_imap_proposition." proposition          " : "").(g:atp_imap_lemma != "" ? g:atp_imap_third_leader.g:atp_imap_lemma." lemma" : "")
		\."\n ".(g:atp_imap_remark != "" ? g:atp_imap_third_leader.g:atp_imap_remark." remark               " : "").(g:atp_imap_corollary != "" ? g:atp_imap_third_leader.g:atp_imap_corollary." corollary" : "")
		\."\n ".(g:atp_imap_proof != "" ? g:atp_imap_third_leader.g:atp_imap_proof." proof                " : "").(g:atp_imap_example != "" ? g:atp_imap_third_leader.g:atp_imap_example." example" : "")
		\."\n ".(g:atp_imap_note != "" ? g:atp_imap_third_leader.g:atp_imap_note." note                 " : "")
		\."\n"
		\."\n ".(g:atp_imap_enumerate != "" ? g:atp_imap_third_leader.g:atp_imap_enumerate." enumerate            " : "").(g:atp_imap_itemize != "" ? g:atp_imap_third_leader.g:atp_imap_itemize." itemize" : "")
		\."\n ".(g:atp_imap_item != "" ? g:atp_imap_third_leader.g:atp_imap_item." \\item" : "")
		\."\n"
		\.(g:atp_imap_align != "" ? "\n ".g:atp_imap_third_leader.g:atp_imap_align." align                " : "").(g:atp_imap_equation != "" ? g:atp_imap_third_leader.g:atp_imap_equation." equation" : "")
		\."\n"
		\."\n ".(g:atp_imap_flushleft != "" ? g:atp_imap_third_leader.g:atp_imap_flushleft." flushleft            " : "").(g:atp_imap_flushright != "" ? g:atp_imap_third_leader.g:atp_imap_flushright." flushright" : "")
		\."\n ".(g:atp_imap_center != "" ? g:atp_imap_third_leader.g:atp_imap_center." center" : "")
		\."\n"
		\.(g:atp_imap_tikzpicture != "" ? "\n ".g:atp_imap_third_leader.g:atp_imap_tikzpicture." tikzpicture" : "")
		\."\n"
		\."\n ".(g:atp_imap_frame != "" ? g:atp_imap_third_leader.g:atp_imap_frame." frame                " : "").(g:atp_imap_letter != "" ?  g:atp_imap_third_leader.g:atp_imap_letter." letter" : "" )
    return g:help_envimaps
endfunction
silent call <SID>HelpEnvIMaps()

" {{{1 Help VMaps
function! <SID>HelpVMaps() 

    if exists("g:no_plugin_maps") || exists("g:no_atp_maps")
	echomsg "[ATP:] ATP maps are turned off"
	return ''
    endif

    let l:atp_vmap_text_font_leader 	= ( exists("maplocalleader") && g:atp_vmap_text_font_leader 	== "<LocalLeader>" ? maplocalleader : g:atp_vmap_text_font_leader )
    let l:atp_vmap_environment_leader 	= ( exists("maplocalleader") && g:atp_vmap_environment_leader 	== "<LocalLeader>" ? maplocalleader : g:atp_vmap_environment_leader )
    let l:atp_vmap_bracket_leader 	= ( exists("maplocalleader") && g:atp_vmap_bracket_leader 	== "<LocalLeader>" ? maplocalleader : g:atp_vmap_bracket_leader )
    let l:atp_vmap_big_bracket_leader 	= ( exists("maplocalleader") && g:atp_vmap_big_bracket_leader 	=~ "<LocalLeader>" ? substitute(g:atp_vmap_big_bracket_leader, '<LocalLeader>', maplocalleader, '')  : g:atp_vmap_big_bracket_leader )

    let g:help_vmaps = ''
	    \."\n <maplocalleader> has value g:atp_vmap_text_font_leader"
	    \."\n KEYMAP            TEXT MODE            MATH MODE"
	    \."\n ".l:atp_vmap_text_font_leader."rm               \\textrm{}            \\mathrm{}"
	    \."\n ".l:atp_vmap_text_font_leader."em               \\emph{}              \\mathit{}"
	    \."\n ".l:atp_vmap_text_font_leader."it               \\textit{}            \\mathit{}"
	    \."\n ".l:atp_vmap_text_font_leader."sf               \\textsf{}            \\mathsf{}"
	    \."\n ".l:atp_vmap_text_font_leader."tt               \\texttt{}            \\mathtt{}"
	    \."\n ".l:atp_vmap_text_font_leader."bf               \\textbf{}            \\mathbf{}"
	    \."\n ".l:atp_vmap_text_font_leader."bb               \\textbf{}            \\mathbb{}"
	    \."\n ".l:atp_vmap_text_font_leader."bb               \\textbf{}            \\mathbb{}"
	    \."\n ".l:atp_vmap_text_font_leader."sl               \\textsl{}"
	    \."\n ".l:atp_vmap_text_font_leader."sc               \\textsc{}"
	    \."\n ".l:atp_vmap_text_font_leader."up               \\textup{}"
	    \."\n ".l:atp_vmap_text_font_leader."md               \\textmd{}"
	    \."\n ".l:atp_vmap_text_font_leader."un               \\underline{}         \\underline{}"
	    \."\n ".l:atp_vmap_text_font_leader."ov               \\overline{}          \\overline{}"
	    \."\n ".l:atp_vmap_text_font_leader."no               \\textnormal{}        \\mathnormal{}"
	    \."\n ".l:atp_vmap_text_font_leader."cal                                   \\mathcal{}"
	    \."\n "
	    \."\n MODE INDEPENDENT VMAPS:"
	    \."\n <maplocalleader> has value g:atp_vmap_environment_leader"
	    \."\n ".l:atp_vmap_environment_leader."C		   wrap in center environment"
	    \."\n ".l:atp_vmap_environment_leader."L		   wrap in flushleft environment"
	    \."\n ".l:atp_vmap_environment_leader."R		   wrap in flushright environment"
	    \."\n ".l:atp_vmap_environment_leader."E		   wrap in equation environment"
	    \."\n ".l:atp_vmap_environment_leader."A		   wrap in align environment"
	    \."\n "
	    \."\n <maplocalleader> has value g:atp_vmap_bracket_leader"
	    \."\n ".l:atp_vmap_bracket_leader."(                (:)            ".l:atp_vmap_bracket_leader.")           (:)" 
	    \."\n ".l:atp_vmap_bracket_leader."[                [:]            ".l:atp_vmap_bracket_leader."]           [:]" 
	    \."\n ".l:atp_vmap_bracket_leader."{                {:}            ".l:atp_vmap_bracket_leader."}           {:}" 
	    \."\n ".l:atp_vmap_bracket_leader."\\{              \\{:\\}           ".l:atp_vmap_bracket_leader."\\}         \\{:\\}" 
	    \."\n m                \\(:\\)           M           \\[:\\] "
	    \."\n "
	    \."\n <maplocalleader> has value g:atp_vmap_big_bracket_leader"
	    \."\n ".l:atp_vmap_big_bracket_leader."(          \\left(:\\right)      ".l:atp_vmap_big_bracket_leader.")     \\left(:\\right)" 
	    \."\n ".l:atp_vmap_big_bracket_leader."[          \\left[:\\right]      ".l:atp_vmap_big_bracket_leader."]     \\left[:\\right]" 
	    \."\n ".l:atp_vmap_big_bracket_leader."{          \\left{:\\right}      ".l:atp_vmap_big_bracket_leader."}     \\left{:\\right}" 
	    \."\n ".l:atp_vmap_big_bracket_leader."\\{        \\left\\{:\\right\\}     ".l:atp_vmap_big_bracket_leader."\\}   \\left\\{:\\right\\}" 
	    \."\n "
	    \."\n ".l:atp_vmap_text_font_leader."f                \\usefont{".g:atp_font_encoding."}{}{}{}\\selectfont" 
    return g:help_vmaps 
endfunction
" {{{1 Help IMaps
" function! <SID>HelpIMaps()
" let tc_imap = maparg("<Tab>  ", 'i') =~# 'atplib#TabCompletion' ? '<Tab>' : 
" 	    \ maparg("<F7>   ", 'i') =~# 'atplib#TabCompletion' ? '<F7>' : ""
" let netc_imap = tc_imap == "<Tab>" ? "<S-Tab>" : tc_imap == "<F7>" ? "<S-F7>" : ""
"     let g:help_imaps = ''
" 	    \."\n <maplocalleader> has value g:atp_vmap_text_font_leader"
" 	    \."\n ".tc_imap."            "."Completion (expert mode)"
" 	    \."\n ".netc_imap."            "."Completion (non-expert mode)"
" endfunction
" silent call <SID>HelpIMaps()
" command! -buffer HelpIMaps :echo <SID>HelpIMaps()
" }}}1
endif

" Commands:
" {{{
command! -buffer HelpMathIMaps 	:echo <SID>HelpMathIMaps()
command! -buffer HelpEnvIMaps 	:echo <SID>HelpEnvIMaps()
command! -buffer HelpVMaps 	:echo <SID>HelpVMaps()
"}}}
" vim:fdm=marker:tw=85:ff=unix:noet:ts=8:sw=4:fdc=1
