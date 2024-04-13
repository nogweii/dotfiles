" Set a default/shipped-with-vim colorscheme if none are
" installed otherwise (see plugins.lua to fix that)
if get(g:, 'colors_name', 'default') == "default"
  colorscheme evening
endif
