" Vim syntax file
" Language:     Uzbl config syntax
" Maintainer:   Mason Larobina <mason.larobina@gmail.com> and grodzik
" Version:      0.1

" To install this syntax file place it in your `~/.vim/syntax/` directory.
" To enable automatic uzbl-config file type detection create a new file
" `~/.vim/ftdetect/uzbl.vim` with the following line inside:
"
"    au BufRead,BufNewFile  ~/.config/uzbl/*  set filetype=uzbl
" Or
"    au BufRead,BufNewFile  ~/.config/uzbl/config  set filetype=uzbl

" Issues:
"   1. Hilighting inside @[]@, @()@, @<>@ and string regions would be nice.
"

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn keyword uzblKeyword back forward scroll reload reload_ign_cache stop
syn keyword uzblKeyword zoom_in zoom_out toggle_zoom_type uri js script
syn keyword uzblKeyword toggle_status spawn sync_spawn sync_sh talk_to_socket
syn keyword uzblKeyword exit search search_reverse search_clear dehilight set
syn keyword uzblKeyword dump_config dump_config_as_events chain print event
syn keyword uzblKeyword request menu_add menu_link_add menu_image_add
syn keyword uzblKeyword menu_editable_add menu_separator menu_link_separator
syn keyword uzblKeyword menu_image_separator menu_editable_separator
syn keyword uzblKeyword menu_remove menu_link_remove menu_image_remove
syn keyword uzblKeyword menu_editable_remove hardcopy include

syn match uzblKeyword /\.\@<!sh\s\+/

" Comments
syn match uzblTodo /TODO:/
syn region uzblComment start=/^#/ end=/$/ contains=uzblTodo

" Comment headings
syn region uzblSection start=/^# ===/ end=/$/
syn region uzblSubSection start=/^# ---/ end=/$/

" Integer and float matching
syn match uzblInt /\d\+/
syn match uzblFloat /\d\+\.\d*/

" Handler arguments
syn match uzblArgs /$\d\+/

" Hex colors
syn match uzblHexCol /#\x\{3}\%(\x\{3}\)\=/

" Matches @INTERNAL_VAR and @{INTERNAL_VAR}
syn match uzblInternalExpand /@[A-Z_]\+/
syn match uzblInternalExpand /@{[A-Z_]\+}/

" Matches $ENVIRON_VAR
syn match uzblEnvironVariable /$[A-Za-z0-9_]\+/

" Matches @some_var and @{some_var}
syn match uzblExpand /@[A-Za-z0-9_]\+/

" Matches @command_alias at the beginning of a line.
syn match uzblMacroExpand /^@[A-Za-z0-9_]\+/
syn match uzblMacroExpand /^@{[A-Za-z0-9_]\+}/

" Matches \, \\, \@var and \@{var}
syn match uzblEscape /\\\\/
syn match uzblEscape /\\@/
syn match uzblEscape /\\@[A-Za-z0-9_]\+/
syn match uzblEscape /\\@{[A-Za-z0-9_]\+}/

" Match @[ xml data ]@ regions
syn region uzblXMLEscape start=+@\[+ end=+\]@+ end=+$+
syn region uzblEscape start=+\\@\[+ end=+\]\\@+

" Match @( shell command )@ regions
syn region uzblShellExec start=+@(+ end=+)@+ end=+$+
syn region uzblEscape start=+\\@(+ end=+)\\@+

" Match @< javascript command >@ regions
syn region uzblJSExec start=+@<+ end=+>@+ end=+$+
syn region uzblEscape start=+\\@<+ end=+>\\@+

" Match quoted regions
syn region uzblString start=+'+ end=+'+ end=+$+ contains=uzblExpand,uzblEscape,uzblHexCol,uzblArgs
syn region uzblString start=+"+ end=+"+ end=+$+ contains=uzblExpand,uzblEscape,uzblHexCol,uzblArgs

if version >= 508 || !exists("did_uzbl_syn_inits")
    if version <= 508
        let did_uzbl_syn_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    HiLink uzblComment Comment
    HiLink uzblTodo Todo

    HiLink uzblSection Folded
    HiLink uzblSubSection SpecialComment

    HiLink uzblKeyword Keyword

    HiLink uzblInt Number
    HiLink uzblFloat Float

    HiLink uzblHexCol Constant

    HiLink uzblArgs Identifier

    HiLink uzblExpand Type
    HiLink uzblMacroExpand Macro
    HiLink uzblEnvironVariable Number
    HiLink uzblInternalExpand Identifier

    HiLink uzblXMLEscape Macro
    HiLink uzblShellExec Macro
    HiLink uzblJSExec Macro

    HiLink uzblEscape Special

    HiLink uzblString String

    delcommand HiLink
endif

let b:current_syntax = 'uzbl'