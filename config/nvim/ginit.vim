" Neovim GUI settings configuration file. Paired with init.vim, so mostly see
" that for information.

" If we're running in NeoVim-GTK...
if exists('g:GtkGuiLoaded')
  " Change the font
  call rpcnotify(1, 'Gui', 'Font', 'SauceCodePro Nerd Font 14')
endif
