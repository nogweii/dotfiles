#!/usr/bin/ruby

require 'date'
require_relative 'patternfly_palette'
require 'erb'
require 'json'

# List of highlight regions, style, and foreground & background colors.
# Where style is one of bold, underline, reverse, italic or nil (vim NONE)
# Where the colors are a symbol from the PatternFly palette

@colors = [
  #'highlight region name',  style,  :foo_400, :bar_100],
  ['Normal',                   nil,    :blue_50,       :black_900],
  ['Cursor',                   nil,    :blue_50,       :blue_50],

  # Various features with a UI
  ['Incsearch',                'bold', :white,       :orange_700],
  ['Search',                   nil,    :black_1000,  :gold_300],

  ['ErrorMsg',                 nil,    :black_1000,  :red_200],
  ['WarningMsg',               nil,    :black_1000,  :light_green_200],
  ['ModeMsg',                  'bold', :green_600,   nil],
  ['MoreMsg',                  'bold', :green_600,   nil],
  ['Question',                 'bold', :green_600,   nil],

  ['StatusLine',               nil,    :gold_500,    :black_1000],
  ['StatusLineNC',             nil,    :gold_500,    :black_850],
  ['VertSplit',                nil,    :black_200,    :light_green_700],
  ['LineNr',                   nil,    :blue_400,  nil],

  ['WildMenu',                 nil,    :green_500,    :black_700],
  # ['Pmenu',                    '',          228, 236, '',          ''],
  # ['PmenuSel',                 'bold',      226, 233, '',          ''],
  # ['PmenuSbar',                '',          119, 16,  '',          ''],
  # ['PmenuThumb',               '',          11,  16,  '',          ''],

  ['DiffText',                 'underline',   :light_blue_400,  nil],
  ['DiffChange',               nil,    nil,  :light_blue_100],
  ['DiffDelete',               nil,    nil,  :red_200],
  ['DiffAdd',                  nil,    nil,  :light_green_300],

  ['Directory',                nil,    :light_blue_400,  nil],
  ['FoldColumn',               nil,    :cyan_500,  nil],
  ['NonText',                  nil,    :black_400, nil],
  ['EndOfBuffer',              nil,    :black_400, nil],
  ['SpecialKey',               nil,    :blue_300, :black_700],
  ['Visual',                   nil,  :black_850, :black_300],
  ['Title',                    nil,  :cyan_400, nil],
  ['Folded',                   nil,  :light_green_100, :black_700],

  # The following are common suggested group names from the Vim documentation:
  ['Comment',                  'italic',  :black_700, nil],

  ['Constant',                 nil,  :green_400, nil],
    ['String',                 nil,  :blue_50, :red_500],
    # ['Character',                   '',          198, 234, '',          ''],
    ['Number',                 nil,  :gold_400,  nil],
    # ['Float',                   '',          50,  '',  '',          ''],
    # ['Boolean',                 nil,  :green_600, nil],

  ['Identifier',               nil,  :light_green_500, nil],
    # ['Function',               '',          196, '',  '',          ''],

  ['Statement',                'bold',  :purple_400, nil],
    # ['Conditional',                '',          69,  '',  '',          ''],
    # ['Repeat',                '',          69,  '',  '',          ''],
    # ['Label',                '',          69,  '',  '',          ''],
    # ['Operator',                '',          69,  '',  '',          ''],
    # ['Keyword',                '',          69,  '',  '',          ''],
    # ['Exception',                '',          69,  '',  '',          ''],

  ['PreProc',                  nil,  :cyan_300, nil],
    # ['Include',                  '',          67,  '',  '',          ''],
    # ['Define',                  '',          67,  '',  '',          ''],
    # ['Macro',                  '',          67,  '',  '',          ''],
    # ['PreCondit',                  '',          67,  '',  '',          ''],

  ['Type',                     nil,   :green_300,  nil],
    # ['StorageClass',                     '',          34,  '',  '',          ''],
    # ['Structure',                     '',          34,  '',  '',          ''],
    # ['Typedef',                     '',          34,  '',  '',          ''],

  ['Special',                  nil,   :green_300, nil],
    ['SpecialChar',            'bold',   :gold_200, :black_700],
    # ['Tag',              '',          155, '',  '',          ''],
    # ['Delimiter',              '',          155, '',  '',          ''],
    # ['SpecialComment',              '',          155, '',  '',          ''],
    # ['Debug',              '',          155, '',  '',          ''],

  ['Underlined',               'underline', :green_300, nil],

  ['Ignore',                   nil,          nil,  nil],

  ['Error',                    nil,         :white, :red_300],

  ['Todo',                     'bold',      :black_800, :gold_400],


  # # Spell checking
  # ['SpellBad',                 'underline', 88,  '',  'undercurl', 160],
  # ['SpellRare',                '',          82,  233, '',          ''],
  # ['SpellLocal',               '',          227, 234, '',          ''],
  # ['SpellCap',                 '',          46,  236, '',          ''],

# # ['color-group', 'term-style', 'foreground-color', 'background-color', 'gui-style', 'under-curl-color' ],
  # ['SignColumn',               'NONE',      '',  233, '',          ''],
  # ['TaglistTagName',           'bold',      48,  124, '',          ''],
  # ['ColorColumn',              '',          11,  239, '',          ''],
  # ['MatchParen',               'bold',      15,  22,  '',          ''],
  # ['TabLine',                  '',          253, 30,  '',          ''],
  # ['TabLineSel',               'bold',      247, 16,  '',          ''],
  # ['TabLineFill',              '',          247, 16,  '',          ''],

  # ['statusColNr',              'NONE',      233, 23,  '',          ''],
  # ['statusColNrPowerline',     'NONE',      23,  234, '',          ''],
  # ['statusColNcPowerline',     'NONE',      23,  233, '',          ''],
  # ['statusFileName',           'NONE',      214, 234, '',          ''],
  # ['statusFileType',           'NONE',      39,  234, '',          ''],
  # ['statusBranch',             'NONE',      202, 234, '',          ''],
  # ['statusFlag',               'NONE',      200, 234, '',          ''],
  # ['diffRemoved',              '',          '',  124, '',          ''],
  # ['ShowMarksHLl',             'bold',      18,  43,  '',          ''],
  # ['ShowMarksHLu',             'bold',      9,   1,   '',          ''],
  # ['ShowMarksHLo',             'bold',      11,  3,   '',          ''],
  # ['ShowMarksHLm',             'bold',      2,   20,  '',          ''],
  # ['cppSTL',                   '',          130, '',  '',          ''],
  # ['rubyRegexpEscape',         '',          162, 234, '',          ''],
  # ['rubyStringDelimiter',      '',          162, '',  '',          ''],
  # ['diffAdded',                '',          '',  22,  '',          ''],
  # ['javaScriptTemplateVar',    '',          172, '',  '',          ''],
  # ['javaScriptTemplateDelim',  'bold',      166, '',  '',          ''],
  # ['javaScriptTemplateString', '',          162, '',  '',          '']
]

@highlight_links = {
  "rubyStringDelimiter" => "String",
  "Quote" => "String",
}

# TODO: might need to include User1 and User2 colors, for the status bar

def maybe_color(val)
  if val.nil?
    return nil
  elsif val == ''
    return 'NONE'
  else
    return val
  end
end


@colouring = Tco::Colouring.new(Tco::Config.new([]))

# Given a color value from the array, it can be one of the following:
#  - An empty string, to purposefully set NONE for the color field
#  - A nil, to skip setting colors for this group
#  - A symbol, to use the patternfly palette
#
# Returns a list of 2 values, the cterm and gui colors. Or false if it's
# supposed to be skipped.
def coerce_color(value)
  case value
  when Symbol
    color_string = $pf_colors[value]
    return [
      @colouring.palette.match_colour(@colouring.get_colour_instance(color_string)),
      color_string
    ]
  when ''
    return ['NONE', 'NONE']
  when nil
    return false
  else
    raise "Don't know how to parse '#{value}'"
  end
end

puts ERB.new(DATA.read, nil, '-<>').result binding

__END__
" Vim colorscheme patternfly.vim
" Last Change: <%= Date.today.strftime("%-d %b %Y") %>
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
<% @colors.each do |color| %>
<% if color[1] != '' && !color[1].nil? %>
highlight <%= color[0] %> cterm=<%= color[1] %> gui=<%= color[1] %>
<% end %>
<% if coerced = coerce_color(color[2]) %>
highlight <%= color[0] %> ctermfg=<%= coerced[0] %> guifg=<%= coerced[1] %>
<% end %>
<% if coerced = coerce_color(color[3]) %>
highlight <%= color[0] %> ctermbg=<%= coerced[0] %> guibg=<%= coerced[1] %>
<% end %>
<% end %>

" }}}

" And a bunch of linked regions, overriding other plugins, language syntax, etc
<% @highlight_links.each do |from, to| %>
highlight link <%= from %> <%= to %>
<% end %>
