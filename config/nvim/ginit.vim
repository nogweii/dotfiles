" Neovim GUI settings configuration file. Paired with init.vim, so mostly see
" that for information.

let font_name = "FantasqueSansMono Nerd Font"
let font_size = 16


" If we're running in NeoVim-GTK...
if exists('g:GtkGuiLoaded')
  " Change the font
  call rpcnotify(1, 'Gui', 'Font', font_name . ' ' . font_size)
endif

if exists('g:GuiLoaded')
  execute 'Guifont! ' . font_name . ':h' . font_size
endif
