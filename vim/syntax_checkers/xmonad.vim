"============================================================================
"File:        xmonad.vim
"Description: Syntax checking plugin for syntastic.vim specifically for
"             when you are editing xmonad.hs.
"Maintainer:  Colin Shea <colin@evaryont.me>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"Ripped From: Anthony Carapetis's haskell.vim
"
"
"============================================================================
if exists("loaded_haskell_syntax_checker") && expand("%:h") != "xmonad.hs"
    finish
endif
let loaded_haskell_syntax_checker = 1

"bail if the user doesn't have xmonad installed
if !executable("xmonad")
    finish
endif

" XMonad uses ghc, which is already slow but xmonad can add a second or two.
" Since we don't have any control over that, we're SOL.
function! SyntaxCheckers_haskell_GetLocList()
    let makeprg = 'xmonad --recompile'
    let errorformat = '%-G\\s%#,%f:%l:%c:%m,%E%f:%l:%c:,%Z%m,'


    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction
