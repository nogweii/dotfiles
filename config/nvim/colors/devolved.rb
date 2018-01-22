#!/usr/bin/ruby

require 'date'
require 'rainbow'
require 'erb'
require 'json'

# ['color-group', 'term-style', 'foreground-color', 'background-color', 'gui-style', 'under-curl-color' ],
@colors = [
  ['Normal',                   'NONE',      253, 233, 'NONE',      'NONE'],
  ['Cursor',                   '',          255, 33,  '',          ''],
  ['CursorLine',               'underline', '',  '',  '',          ''],
  ['CursorColumn',             '',          '',  223, '',          ''],
  ['Incsearch',                'bold',      195, 124, '',          ''],
  ['Search',                   '',          232, 184, '',          ''],
  ['ErrorMsg',                 'bold',      16,  202, '',          ''],
  ['WarningMsg',               'bold',      16,  190, '',          ''],
  ['ModeMsg',                  'bold',      226, 18,  '',          ''],
  ['MoreMsg',                  'bold',      16,  154, '',          ''],
  ['Question',                 'bold',      70,  '',  '',          ''],
  ['StatusLine',               'bold',      84,  234, '',          ''],
  ['StatusLineNC',             '',          41,  233, '',          ''],
  ['User1',                    'bold',      28,  '',  '',          ''],
  ['User2',                    'bold',      39,  '',  '',          ''],
  ['VertSplit',                '',          84,  22,  '',          ''],
  ['WildMenu',                 'bold',      208, '',  '',          ''],
  ['DiffText',                 '',          16,  190, '',          ''],
  ['DiffChange',               '',          18,  83,  '',          ''],
  ['DiffDelete',               '',          79,  124, '',          ''],
  ['DiffAdd',                  '',          79,  21,  '',          ''],
  ['Folded',                   'bold',      38,  234, '',          ''],
  ['FoldedColumn',             '',          39,  190, '',          ''],
  ['FoldColumn',               '',          38,  234, '',          ''],
  ['Directory',                '',          28,  '',  '',          ''],
  ['LineNr',                   'NONE',      23,  233, '',          ''],
  ['NonText',                  '',          244, '',  '',          ''],
  ['SpecialKey',               '',          190, '',  '',          ''],
  ['Title',                    'bold',      98,  '',  '',          ''],
  ['Visual',                   '',          '',  238, '',          ''],
  ['Comment',                  'italic',    240, '',  '',          ''],
  ['Constant',                 '',          75,  '',  '',          ''],
  ['Include',                  '',          67,  '',  '',          ''],
  ['String',                   '',          198, 234, '',          ''],
  ['Error',                    '',          69,  '',  '',          ''],
  ['Identifier',               '',          196, '',  '',          ''],
  ['Ignore',                   '',          '',  '',  '',          ''],
  ['Number',                   '',          50,  '',  '',          ''],
  ['PreProc',                  '',          62,  '',  '',          ''],
  ['SignColumn',               'NONE',      '',  233, '',          ''],
  ['Special',                  '',          15,  234, '',          ''],
  ['SpecialChar',              '',          155, '',  '',          ''],
  ['Statement',                '',          69,  '',  '',          ''],
  ['Todo',                     'bold',      16,  148, '',          ''],
  ['Type',                     '',          34,  '',  '',          ''],
  ['Underlined',               'bold',      77,  '',  '',          ''],
  ['TaglistTagName',           'bold',      48,  124, '',          ''],
  ['ColorColumn',              '',          11,  239, '',          ''],
  ['Pmenu',                    '',          228, 236, '',          ''],
  ['PmenuSel',                 'bold',      226, 233, '',          ''],
  ['PmenuSbar',                '',          119, 16,  '',          ''],
  ['PmenuThumb',               '',          11,  16,  '',          ''],
  ['SpellBad',                 'underline', 88,  '',  'undercurl', 160],
  ['SpellRare',                '',          82,  233, '',          ''],
  ['SpellLocal',               '',          227, 234, '',          ''],
  ['SpellCap',                 '',          46,  236, '',          ''],
  ['MatchParen',               'bold',      15,  22,  '',          ''],
  ['TabLine',                  '',          253, 30,  '',          ''],
  ['TabLineSel',               'bold',      247, 16,  '',          ''],
  ['TabLineFill',              '',          247, 16,  '',          ''],
  ['statusColNr',              'NONE',      233, 23,  '',          ''],
  ['statusColNrPowerline',     'NONE',      23,  234, '',          ''],
  ['statusColNcPowerline',     'NONE',      23,  233, '',          ''],
  ['statusFileName',           'NONE',      214, 234, '',          ''],
  ['statusFileType',           'NONE',      39,  234, '',          ''],
  ['statusBranch',             'NONE',      202, 234, '',          ''],
  ['statusFlag',               'NONE',      200, 234, '',          ''],
  ['diffRemoved',              '',          '',  124, '',          ''],
  ['ShowMarksHLl',             'bold',      18,  43,  '',          ''],
  ['ShowMarksHLu',             'bold',      9,   1,   '',          ''],
  ['ShowMarksHLo',             'bold',      11,  3,   '',          ''],
  ['ShowMarksHLm',             'bold',      2,   20,  '',          ''],
  ['cppSTL',                   '',          130, '',  '',          ''],
  ['rubyRegexpEscape',         '',          162, 234, '',          ''],
  ['rubyStringDelimiter',      '',          162, '',  '',          ''],
  ['diffAdded',                '',          '',  22,  '',          ''],
  ['javaScriptTemplateVar',    '',          172, '',  '',          ''],
  ['javaScriptTemplateDelim',  'bold',      166, '',  '',          ''],
  ['javaScriptTemplateString', '',          162, '',  '',          '']
]

def maybe_none(val)
  if val.nil? || val == ''
    'NONE'
  else
    val
  end
end

def fg_text(val)
  if maybe_none(val) == 'NONE'
    @colors[0][2]
  else
    val
  end
end

@data = JSON.parse(open('data.json').read)
def handle_color(color)
  return 'NONE' if color.nil? || color == '' || color == 'NONE'
  @data.each do |xterm_color|
    return xterm_color['hexString'] if xterm_color['colorId'] == color
  end
  "COLOR_ID_MISSING: #{color}"
end

#       exec "hi ".a:colarg[0]." gui=".guival." guifg=".fg." guibg=".bg." guisp=".sp
#       exec "hi ".s:colorRow[0]." cterm=".s:colorRow[1]." ctermfg=".s:colorRow[2]." ctermbg=".s:colorRow[3]

puts ERB.new(DATA.read, nil, '-<>').result binding

__END__
" Vim colorscheme devolved.vim
" Last Change: <%= Date.today.strftime("%-d %b %Y") %>
" License: Public domiain, or CC0. No rights reserved, no warranty either though.
" Maintainer: Evaryont <hello@evaryont.me>

" This is the result of the devolved.rb script. Don't make changes directly,
" they will be overwritten.

if !has("gui_running") && &t_Co != 88 && &t_Co != 256
    finish
endif

if exists("syntax_on")
    syntax reset
endif

let g:colors_name = "devolved"

<% @colors.each do |color| -%>
highlight <%= color[0] %> cterm=<%= maybe_none(color[1]) %> ctermfg=<%= fg_text(color[2]) %> ctermbg=<%= maybe_none(color[3]) %>
highlight <%= color[0] %> gui=<%= maybe_none(color[5]) %> guifg=<%= handle_color(fg_text(color[2])) %> guibg=<%= handle_color(color[3]) %> guisp=<%= handle_color(color[5]) %>
<% end -%>
