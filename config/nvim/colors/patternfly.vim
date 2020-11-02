" Vim colorscheme patternfly.vim
" Last Change: 6 May 2020
" License: Public domiain, or CC0. No rights reserved, no warranty either though.
" Maintainer: Evaryont <hello@evaryont.me>

" A colorscheme using only the patternfly v4 color palette. This is the
" result of the patternfly.rb script. Don't make changes directly, they will be
" overwritten.

if !has("gui_running") && &t_Co != 256
    " Must use a GUI, or have 256 color support in your terminal, bail out
    " early if that's not the case
    finish
endif

if exists("syntax_on")
    syntax reset
endif

let g:colors_name = "patternfly"

" The list of generated highlight commands {{{



highlight Normal ctermfg=255 guifg=#DEF3FF


highlight Normal ctermbg=16 guibg=#030303




highlight Cursor ctermfg=255 guifg=#DEF3FF


highlight Cursor ctermbg=255 guibg=#DEF3FF



highlight Incsearch cterm=bold gui=bold


highlight Incsearch ctermfg=231 guifg=#FFFFFF


highlight Incsearch ctermbg=52 guibg=#3B1F00




highlight Search ctermfg=16 guifg=#030303


highlight Search ctermbg=221 guibg=#F4C145




highlight ErrorMsg ctermfg=16 guifg=#030303


highlight ErrorMsg ctermbg=124 guibg=#A30000




highlight WarningMsg ctermfg=16 guifg=#030303


highlight WarningMsg ctermbg=192 guibg=#C8EB79



highlight ModeMsg cterm=bold gui=bold


highlight ModeMsg ctermfg=22 guifg=#1E4F18




highlight MoreMsg cterm=bold gui=bold


highlight MoreMsg ctermfg=22 guifg=#1E4F18




highlight Question cterm=bold gui=bold


highlight Question ctermfg=22 guifg=#1E4F18





highlight StatusLine ctermfg=136 guifg=#C58C00


highlight StatusLine ctermbg=16 guibg=#030303




highlight StatusLineNC ctermfg=136 guifg=#C58C00


highlight StatusLineNC ctermbg=235 guibg=#212427




highlight VertSplit ctermfg=255 guifg=#EDEDED


highlight VertSplit ctermbg=22 guibg=#253600




highlight LineNr ctermfg=26 guifg=#0066CC





highlight WildMenu ctermfg=65 guifg=#467F40


highlight WildMenu ctermbg=239 guibg=#4F5255



highlight DiffText cterm=underline gui=underline


highlight DiffText ctermfg=38 guifg=#00B9E4






highlight DiffChange ctermbg=195 guibg=#BEEDF9





highlight DiffDelete ctermbg=124 guibg=#A30000





highlight DiffAdd ctermbg=148 guibg=#ACE12E




highlight Directory ctermfg=38 guifg=#00B9E4





highlight FoldColumn ctermfg=23 guifg=#003737





highlight NonText ctermfg=250 guifg=#B8BBBE





highlight EndOfBuffer ctermfg=250 guifg=#B8BBBE





highlight SpecialKey ctermfg=75 guifg=#2B9AF3


highlight SpecialKey ctermbg=239 guibg=#4F5255




highlight Visual ctermfg=235 guifg=#212427


highlight Visual ctermbg=252 guibg=#D2D2D2




highlight Title ctermfg=23 guifg=#005F60





highlight Folded ctermfg=193 guifg=#E4F5BC


highlight Folded ctermbg=239 guibg=#4F5255



highlight Comment cterm=italic gui=italic


highlight Comment ctermfg=239 guifg=#4F5255





highlight Constant ctermfg=71 guifg=#5BA352





highlight String ctermfg=255 guifg=#DEF3FF


highlight String ctermbg=52 guibg=#2C0000




highlight Number ctermfg=214 guifg=#F0AB00





highlight Identifier ctermfg=70 guifg=#6CA100




highlight Statement cterm=bold gui=bold


highlight Statement ctermfg=104 guifg=#8476D1





highlight PreProc ctermfg=30 guifg=#009596





highlight Type ctermfg=77 guifg=#6EC664





highlight Special ctermfg=77 guifg=#6EC664




highlight SpecialChar cterm=bold gui=bold


highlight SpecialChar ctermfg=222 guifg=#F6D173


highlight SpecialChar ctermbg=239 guibg=#4F5255



highlight Underlined cterm=underline gui=underline


highlight Underlined ctermfg=77 guifg=#6EC664









highlight Error ctermfg=231 guifg=#FFFFFF


highlight Error ctermbg=88 guibg=#7D1007



highlight Todo cterm=bold gui=bold


highlight Todo ctermfg=237 guifg=#3C3F42


highlight Todo ctermbg=214 guibg=#F0AB00



" }}}

" And a bunch of linked regions, overriding other plugins, language syntax, etc

highlight link rubyStringDelimiter String

highlight link Quote String

