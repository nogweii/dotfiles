" Vim color file
" Name:       inkpot.vim
" Maintainer: Ciaran McCreesh <ciaran.mccreesh@blueyonder.co.uk>
" This should work in the GUI, rxvt-unicode (88 colour mode) and xterm (256
" colour mode). It won't work in 8/16 colour terminals.
"
" To use a black background, :let g:inkpot_black_background = 1

set background=dark
hi clear
if exists("syntax_on")
        syntax reset
endif

let colors_name = "inkpit_ct"


hi Normal         cterm=NONE   ctermfg=79 ctermbg=16

hi IncSearch      cterm=BOLD   ctermfg=80 ctermbg=73
hi Search         cterm=NONE   ctermfg=80 ctermbg=73
hi ErrorMsg       cterm=BOLD   ctermfg=16 ctermbg=48
hi WarningMsg     cterm=BOLD   ctermfg=16 ctermbg=68
hi ModeMsg        cterm=BOLD   ctermfg=38 ctermbg=NONE
hi MoreMsg        cterm=BOLD   ctermfg=38 ctermbg=NONE
hi Question       cterm=BOLD   ctermfg=52 ctermbg=NONE

hi StatusLine     cterm=BOLD   ctermfg=85 ctermbg=81
hi User1          cterm=BOLD   ctermfg=28 ctermbg=81
hi User2          cterm=BOLD   ctermfg=39 ctermbg=81
hi StatusLineNC   cterm=NONE   ctermfg=84 ctermbg=81
hi VertSplit      cterm=NONE   ctermfg=84 ctermbg=81

hi WildMenu       cterm=BOLD   ctermfg=87 ctermbg=38

hi MBENormal                   ctermfg=85 ctermbg=81
hi MBEChanged                  ctermfg=87 ctermbg=81
hi MBEVisibleNormal            ctermfg=85 ctermbg=82
hi MBEVisibleChanged           ctermfg=87 ctermbg=82

hi DiffText       cterm=NONE   ctermfg=79 ctermbg=34
hi DiffChange     cterm=NONE   ctermfg=79 ctermbg=17
hi DiffDelete     cterm=NONE   ctermfg=79 ctermbg=32
hi DiffAdd        cterm=NONE   ctermfg=79 ctermbg=20

hi Folded         cterm=NONE   ctermfg=79 ctermbg=35
hi FoldColumn     cterm=NONE   ctermfg=39 ctermbg=80

hi Directory      cterm=NONE   ctermfg=28 ctermbg=NONE
hi LineNr         cterm=NONE   ctermfg=39 ctermbg=80
hi NonText        cterm=BOLD   ctermfg=39 ctermbg=NONE
hi SpecialKey     cterm=BOLD   ctermfg=55 ctermbg=NONE
hi Title          cterm=BOLD   ctermfg=48 ctermbg=NONE
hi Visual         cterm=NONE   ctermfg=79 ctermbg=38

hi Comment        cterm=NONE   ctermfg=52 ctermbg=NONE
hi Constant       cterm=NONE   ctermfg=73 ctermbg=NONE
hi String         cterm=NONE   ctermfg=73 ctermbg=81
hi Error          cterm=NONE   ctermfg=79 ctermbg=32
hi Identifier     cterm=NONE   ctermfg=53 ctermbg=NONE
hi Ignore         cterm=NONE
hi Number         cterm=NONE   ctermfg=69 ctermbg=NONE
hi PreProc        cterm=NONE   ctermfg=25 ctermbg=NONE
hi Special        cterm=NONE   ctermfg=55 ctermbg=NONE
hi SpecialChar    cterm=NONE   ctermfg=55 ctermbg=81
hi Statement      cterm=NONE   ctermfg=27 ctermbg=NONE
hi Todo           cterm=BOLD   ctermfg=16 ctermbg=57
hi Type           cterm=NONE   ctermfg=71 ctermbg=NONE
hi Underlined     cterm=BOLD   ctermfg=77 ctermbg=NONE
hi TaglistTagName cterm=BOLD   ctermfg=39 ctermbg=NONE

if v:version >= 700
        hi Pmenu          cterm=NONE   ctermfg=87 ctermbg=82
        hi PmenuSel       cterm=BOLD   ctermfg=87 ctermbg=38
        hi PmenuSbar      cterm=BOLD   ctermfg=87 ctermbg=39
        hi PmenuThumb     cterm=BOLD   ctermfg=87 ctermbg=39

        hi SpellBad       cterm=NONE ctermbg=32
        hi SpellRare      cterm=NONE ctermbg=33
        hi SpellLocal     cterm=NONE ctermbg=36
        hi SpellCap       cterm=NONE ctermbg=21
        hi MatchParen     cterm=NONE ctermbg=14 ctermfg=25
endif

" vim: set et :
