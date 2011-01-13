" ============================================================================
" File:        NERD_Tree_search.vim
" Description: Adds searching capabilities to NERD_Tree
" Maintainer:  Tudor Barbu <miau at motane dot lu>
" Last Change: 2010-12-08
" License:     This program is free software. It comes without any warranty,
"              to the extent permitted by applicable law. You can redistribute
"              it and/or modify it under the terms of the Do What The Fuck You
"              Want To Public License, Version 2, as published by Sam Hocevar.
"              See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" ============================================================================


" don't load multiple times
if exists("g:loaded_nerdtree_ack")
    finish
endif

let g:loaded_nerdtree_ack = 1

if !exists("g:path_to_search_app")
    let g:path_to_search_app = "ack"
endif

let g:path_to_search_app = g:path_to_search_app . "\\ -H\\ --nocolor\\ --nogroup"

" add the new menu item via NERD_Tree's API
call NERDTreeAddMenuItem({
    \ 'text': '(s)earch directory',
    \ 'shortcut': 's',
    \ 'callback': 'NERDTreeAck' })

function! NERDTreeAck()
    " get the current dir from NERDTree
    let cd = g:NERDTreeDirNode.GetSelected().path.str()

    " get the pattern
    let pattern = input("Enter the pattern: ")
    if pattern == ''
        echo 'Maybe another time...'
        return
    endif

    " display first result in the last window
    wincmd w

    let grepprg_bak = &grepprg
    exec "set grepprg=" . g:path_to_search_app
    exec 'silent! grep ' . pattern . ' ' . cd

    let &grepprg=grepprg_bak
    exec "redraw!"

    let hits = len(getqflist())
    if hits == 0
        echo 'Pattern ' . pattern . ' not found in ' . cd
    elseif hits > 1
        echo 'Found ' . hits . ' hits. Use the menu to navigate!'
        botright copen
    endif

endfunction

