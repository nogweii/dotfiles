" First off, get rid of snipMate's bindings...
iunmap <tab>
iunmap <s-tab>
iunmap <c-r><tab>

" After that run, the only map left from superTab is..
iunmap <c-tab>

" delimitMate uses S-Tab to quicky escape, but that interferes with snipMate,
" so let's disable that.
let g:delimitMate_tab2exit = 0

" NOTE: snipMate's tab bindings in select mode are left alone, as generally
" speaking, you want that.

imap <expr> <tab>   ForwardsTab()
imap <expr> <s-tab> BackwardsTab()

" Both plugins want tab to go forwards, and shift-tab to go backwards, looping
" through their functionality. SnipMate does try to play nice with SuperTab,
" but it's not nice enough, imo. If it finds a snippet it expands that, which
" is unwanted. If I want to expand a snippet, I'll do that! Instead, just give
" me the damn completion popup! So, let's do that.

" Regardless of whatever prior text there is, try doing SuperTab's completion.
" Default, if nothing is happening: Return <c-n>, for SuperTab
function ForwardsTab()
    if pumvisible()
        return "\<c-n>"
    elseif exists('g:snipPos')
        return "\<c-r>=TriggerSnippet()\<CR>"
    else
        return "\<c-n>"
    endif
endfunction

" Depending on the prior text, either expand the snippet, or pop up the
" SuperTab completion menu. (SnipMate does that, in trying to play nice)
" Default, if nothing is happening: Return TriggerSnippet(), for snipMate
function BackwardsTab()
    if pumvisible()
        return "\<c-p>"
    elseif exists('g:snipPos')
        return "\<c-r>=BackwardsSnippet()\<cr>"
    else
        return "\<c-r>=TriggerSnippet()\<CR>"
    endif
endfunction
