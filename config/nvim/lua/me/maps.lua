vim.g.mapleader = ';'

-- A little utility function to make nvim_set_keymap a bit more ergonomic
function map(args)
  args = vim.tbl_extend("keep", args, {
    mode = "", -- The vim mode for this map
    keys = nil, -- The input sequence of keys to activate this mapping
    to = nil, -- Sequence to keys to 'type' into the editor

    recurse = false, -- set to true to not include noremap
    silent = true, -- set to false to not include <silent>, e.g. the map will not be echoed to the command line
    expression = false, -- set to true if the output is to be evaluated rather than typed
    plugins = false, -- set to true if the mapping requires plugins and should be disabled when packer wasn't loaded
  })

  if (args.plugins) and (not packer_exists) then
    return
  end

  vim.api.nvim_set_keymap(args.mode, args.keys, args.to, { noremap = not args.recurse, silent = args.silent, expr = args.expression })
end

local function plug_map(args)
  map{keys = args.keys, to = '<Plug>(' .. args.command .. ')', mode = args.mode or 'n', silent = true, recurse = true, plugins = true}
end
local function cmd_map(args)
  map{keys = args.keys, to = '<cmd>' .. args.command .. '<CR>', silent = true, command = true, plugins = true}
end

-- Free up 'G' to be a generic prefix, and make gG do what G used to do
map{keys = "gG", to = "G"}
map{keys = "gG", to = "G", mode = "o"}
map{keys = "G", to = '', recurse = true}

-- change Y to only select the line starting from the cursor
map{keys = "Y", to = "y$"}

-- nop-out semicolon, it's my mapleader key
map{keys = ";", to = '', recurse = true}
-- but restore the functionality by using the comma key
map{keys = ",", to = ";"}

-- Easily (un)indent again in visual mode by immediately re-selecting
map{keys = "<", to = "<gv", mode = "v"}
map{keys = ">", to = ">gv", mode = "v"}

-- Fuzzy find a file to edit
plug_map{keys = "ZE", command = 'CommandT'}
plug_map{keys = "ZB", command = 'CommandTBuffer'}
-- Save the file only when the buffer has been modified.
cmd_map{keys = "ZD", command = "BufferWipeout"}
cmd_map{keys = "ZW", command = "update"}
-- Create & edit a snippets file for this filetype
cmd_map{keys = "ZP", command = "UltiSnipsEdit"}
-- Easily edit my vimrc file
-- TODO: integrate it with my workspace concept, editing a project-local lua file instead
cmd_map{keys = "ZL", command = "edit " .. vim.fn.stdpath("config") .. "/init.lua"}

-- Sometimes you just need to move a character or two in insert mode. Don't
-- make these a habit, though!
map{keys = "<C-j>", to = "<Down>",  mode = "i"}
map{keys = "<C-k>", to = "<Up>",    mode = "i"}
map{keys = "<C-h>", to = "<Left>",  mode = "i"}
map{keys = "<C-l>", to = "<Right>", mode = "i"}

-- Swap ` and ', making ' more precise (line & column) by default
map{keys = "`", to = "'"}
map{keys = "'", to = "`"}

-- Make interacting with the spell checking a little easier
map{keys = "zP", to = ":set spell!<CR>"} -- Toggle spell check quickly
map{keys = "zp", to = "1z="} -- Accept the first spell correction

-- Attempt to clear the screen of artifacts and clear search highlight
map{keys = "<C-l>", to = "<c-l>:nohlsearch<CR>:redraw<CR>", recurse = true}

-- Map H and L to jump to the beginning or end of the line. H is smarter, in
-- that it jumps between the first non-whitespace character or actual column 0.
map{keys = "H", to = "(col('.') == matchend(getline('.'), '^\\s*')+1 ? '0' : '^')", expression = true}
map{keys = "L", to = '$', recurse = true}

-- Easily get out of insert mode in the terminal
map{keys = "<C-s>", to = '<C-\\><C-n>', recurse = true, mode = "t"}

-- Tap - to jump into a file pane
cmd_map{keys = "-", command = "NvimTreeToggle", plugins = true}

-- Git hunk jumps, that behave the same when diffing two files
map{keys = "]c", to = "&diff ? ']c' : '<cmd>lua require('gitsigns').next_hunk()<CR>'", expression = true}
map{keys = "[c", to = "&diff ? ']c' : '<cmd>lua require('gitsigns').prev_hunk()<CR>'", expression = true}

-- a much smarter <C-a> and <C-x> that know how to flip through enumerated lists
-- and manipulate additional number formats & dates
plug_map{keys = "<C-a>", command = 'dial-increment'}
plug_map{keys = "<C-x>", command = 'dial-decrement'}
plug_map{mode = 'v', keys = "<C-a>", command = 'dial-increment'}
plug_map{mode = 'v', keys = "<C-x>", command = 'dial-decrement'}
plug_map{mode = 'v', keys = "g<C-a>", command = 'dial-increment'}
plug_map{mode = 'v', keys = "g<C-x>", command = 'dial-decrement'}

-- completion key bindings
map{mode = 'i', keys = "<C-Space>", to = [[compe#complete()]], expression = true, plugins = true}
map{mode = 'i', keys = "<CR>", to = [[compe#confirm({ 'keys': "\<Plug>delimitMateCR", 'mode': '' })]], expression = true, plugins = true}
map{mode = 'i', keys = "<C-e>", to = [[compe#close('<End>')]], expression = true, plugins = true}
-- TODO: what are these even useful for?
-- map{mode = 'i', keys = "<C-f>", to = [[compe#scroll({ 'delta': +4 })]], expression = true}
-- map{mode = 'i', keys = "<C-b>", to = [[compe#scroll({ 'delta': -4 })]], expression = true}

-- Inspired by tpope's rsi.vim, buch much more constrained:
-- jump to the beginning of the line
map{mode = 'i', keys = "<C-a>", to = "<C-o>H", recurse = true}
map{mode = 'c', keys = "<C-a>", to = "<Home>"}
-- jump to the end of the line
-- (command mode already has this binding)
-- (insert mode is covered by compe falling back to <End> in the mapping above)

local is_prior_char_whitespace = function()
  local col = vim.fn.col('.') - 1
  if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
    return true
  else
    return false
  end
end

-- Use (shift-)tab to:
--- move to prev/next item in completion menu
--- jump to the prev/next snippet placeholder
--- insert a simple tab
--- start the completion menu
_G.tab_completion = function()
  if vim.fn.pumvisible() == 1 then
    return vim.api.nvim_replace_termcodes("<C-n>", true, true, true)

  elseif vim.fn["UltiSnips#CanExpandSnippet"]() == 1 or vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
    return vim.api.nvim_replace_termcodes("<C-R>=UltiSnips#ExpandSnippetOrJump()<CR>", true, true, true)

  elseif is_prior_char_whitespace() then
    return vim.api.nvim_replace_termcodes("<Tab>", true, true, true)

  else
    return vim.fn['compe#complete']()
  end
end
_G.shift_tab_completion = function()
  if vim.fn.pumvisible() == 1 then
    return vim.api.nvim_replace_termcodes("<C-p>", true, true, true)

  elseif vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
    return vim.api.nvim_replace_termcodes("<C-R>=UltiSnips#JumpBackwards()<CR>", true, true, true)

  else
    return vim.api.nvim_replace_termcodes("<S-Tab>", true, true, true)
  end
end

map{mode = 'i', keys = "<Tab>", to = [[v:lua.tab_completion()]], expression = true, plugins = true}
map{mode = 's', keys = "<Tab>", to = [[v:lua.tab_completion()]], expression = true, plugins = true}
map{mode = 'i', keys = "<S-Tab>", to = [[v:lua.shift_tab_completion()]], expression = true, plugins = true}
map{mode = 's', keys = "<S-Tab>", to = [[v:lua.shift_tab_completion()]], expression = true, plugins = true}

-- A basic list of all of the known snippets for the buffer
-- TODO: this isn't an interactive menu, it's a big rough for now
map{mode = 'i', keys = "<C-x><C-p>", to = "<C-R>=UltiSnips#ListSnippets()<cr>"}
