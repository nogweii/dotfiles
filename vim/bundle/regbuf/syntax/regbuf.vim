
if v:version < 600
    syntax clear
elseif exists('b:current_syntax')
    finish
endif



syn case match
syn match regbufRegisterName /^"./



if v:version >= 508 || !exists("did_regbuf_syn_inits")
    if v:version < 508
        let did_regbuf_syn_inits = 1
        command -nargs=+ RegbufHiLink hi link <args>
    else
        command -nargs=+ RegbufHiLink hi def link <args>
    endif

    RegbufHiLink regbufRegisterName Identifier

    delcommand RegbufHiLink
endif



let b:current_syntax = 'regbuf'
