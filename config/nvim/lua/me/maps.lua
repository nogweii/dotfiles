vim.g.mapleader = ';'

--[[
 _          _
| |__   ___| |_ __   ___ _ __ ___
| '_ \ / _ \ | '_ \ / _ \ '__/ __|
| | | |  __/ | |_) |  __/ |  \__ \
|_| |_|\___|_| .__/ \___|_|  |___/
             |_|
 ]]
-- A little utility function to make nvim_set_keymap a bit more ergonomic
local function map(args)
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
  map{keys = args.keys, to = '<cmd>' .. args.command .. '<CR>', mode = args.mode or 'n', silent = true, plugins = args.plugins == nil and true or args.plugins}
end

--[[
                                 _
  __ _  ___ _ __   ___ _ __ __ _| |
 / _` |/ _ \ '_ \ / _ \ '__/ _` | |
| (_| |  __/ | | |  __/ | | (_| | |
 \__, |\___|_| |_|\___|_|  \__,_|_|
 |___/
 ]]
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
-- Easily search the directory
cmd_map{keys = "ZG", command = "Grepper"}
plug_map{mode = "o", keys = "gs", command = "GrepperOperator"}
plug_map{mode = "x", keys = "gs", command = "GrepperOperator"}
-- Visualize the undo history of the file
cmd_map{keys = "ZU", command = "MundoToggle"}
-- Find all of the code tags (TODO, NOTE, FIXME, etc) in the project
cmd_map{keys = "ZT", command = "CodeTagSearch"}

-- Tap - to jump into a file pane
cmd_map{keys = "-", command = "NvimTreeToggle"}

-- a much smarter <C-a> and <C-x> that know how to flip through enumerated lists
-- and manipulate additional number formats & dates
plug_map{keys = "<C-a>", command = 'dial-increment'}
plug_map{keys = "<C-x>", command = 'dial-decrement'}
plug_map{mode = 'v', keys = "<C-a>", command = 'dial-increment'}
plug_map{mode = 'v', keys = "<C-x>", command = 'dial-decrement'}
plug_map{mode = 'v', keys = "g<C-a>", command = 'dial-increment'}
plug_map{mode = 'v', keys = "g<C-x>", command = 'dial-decrement'}


-- Inspired by tpope's rsi.vim, buch much more constrained:
-- jump to the beginning of the line
map{mode = 'i', keys = "<C-a>", to = "<C-o>H", recurse = true}
map{mode = 'c', keys = "<C-a>", to = "<Home>"}
-- jump to the end of the line
-- (command mode already has this binding)
-- (insert mode is covered by compe falling back to <End> in the mapping above)

-- easy buffer switching, that's barbar-aware
if packer_exists then
  cmd_map{keys = "<C-n>", command = "BufferNext"}
  cmd_map{keys = "<C-p>", command = "BufferPrev"}
else
  cmd_map{keys = "<C-n>", command = "bnext", plugins = false}
  cmd_map{keys = "<C-p>", command = "bprev", plugins = false}
end


--[[
       _ _
  __ _(_) |_
 / _` | | __|
| (_| | | |_
 \__, |_|\__|
 |___/
 ]]
-- Git hunk jumps, that behave the same when diffing two files
map{keys = "]c", to = "&diff ? ']c' : '<cmd>lua require('gitsigns').next_hunk()<CR>'", expression = true}
map{keys = "[c", to = "&diff ? ']c' : '<cmd>lua require('gitsigns').prev_hunk()<CR>'", expression = true}
-- a motion to select the whole hunk
map{mode = "o", keys = "ih", to = "<cmd>lua require('gitsigns').select_hunk()<CR>"}
map{mode = "x", keys = "ih", to = "<cmd>lua require('gitsigns').select_hunk()<CR>"}
cmd_map{keys = "<leader>gb", command = "GitMessenger"}
map{keys = "<leader>gS", to = "<cmd>lua require('gitsigns').stage_hunk()<CR>"}
map{keys = "<leader>gU", to = "<cmd>lua require('gitsigns').undo_stage_hunk()<CR>"}
map{keys = "<leader>gp", to = "<cmd>lua require('gitsigns').preview_hunk()<CR>"}


--[[
                           _      _   _
  ___ ___  _ __ ___  _ __ | | ___| |_(_) ___  _ __
 / __/ _ \| '_ ` _ \| '_ \| |/ _ \ __| |/ _ \| '_ \
| (_| (_) | | | | | | |_) | |  __/ |_| | (_) | | | |
 \___\___/|_| |_| |_| .__/|_|\___|\__|_|\___/|_| |_|
                    |_|
 ]]
-- completion key bindings
map{mode = 'i', keys = "<ESC>", to = [[pumvisible() ? "<C-e><ESC>" : "<ESC>"]], expression = true}
map{mode = 'i', keys = "<C-c>", to = [[pumvisible() ? "<C-e><C-c>" : "<C-c>"]], expression = true}
-- TODO: integrate nvim-autopairs and replace these two bindings
map{mode = 'i', keys = "<CR>", to = [[pumvisible() ? (complete_info().selected == -1 ? "<C-e><CR>" : "<C-y>") : "<CR>"]], expression = true}
map{mode = 'i', keys = "<BS>", to = [[pumvisible() ? "<C-e><BS>" : "<BS>"]], expression = true}

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
--- jump to the prev/next snippet placeholder (TOOD: how to activate this in coq?)
--- insert a simple tab
--- start the completion menu (TOOD: how to activate this in coq?)
_G.tab_completion = function()
  if vim.fn.pumvisible() == 1 then
    return vim.api.nvim_replace_termcodes("<C-n>", true, true, true)

  elseif is_prior_char_whitespace() then
    return vim.api.nvim_replace_termcodes("<Tab>", true, true, true)
  end
end
_G.shift_tab_completion = function()
  if vim.fn.pumvisible() == 1 then
    return vim.api.nvim_replace_termcodes("<C-p>", true, true, true)

  else
    return vim.api.nvim_replace_termcodes("<S-Tab>", true, true, true)
  end
end

map{mode = 'i', keys = "<Tab>", to = [[v:lua.tab_completion()]], expression = true, plugins = true}
map{mode = 's', keys = "<Tab>", to = [[v:lua.tab_completion()]], expression = true, plugins = true}
map{mode = 'i', keys = "<S-Tab>", to = [[v:lua.shift_tab_completion()]], expression = true, plugins = true}
map{mode = 's', keys = "<S-Tab>", to = [[v:lua.shift_tab_completion()]], expression = true, plugins = true}

--[[
 _ _       _
| (_)_ __ | |_ ___ _ __
| | | '_ \| __/ _ \ '__|
| | | | | | ||  __/ |
|_|_|_| |_|\__\___|_|
 ]]
-- linter errors & LSP diagnostics management via ALE
cmd_map{keys = "[d", command = "ALEPreviousWrap"}
cmd_map{keys = "]d", command = "ALENextWrap"}
cmd_map{keys = "<leader>df", command = "ALEFix"}
cmd_map{keys = "<leader>dd", command = "ALEDetail"}
cmd_map{keys = "<leader>dl", command = "ALELint"}
cmd_map{keys = "<leader>dL", command = "ALEToggle"}

-- return a table so that other files can use these
return {
  map = map,
  plug_map = plug_map,
  cmd_map = cmd_map
}
