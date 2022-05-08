vim.g.mapleader = ';'

local map = require("me.map_utils").map
local cmd_map = require("me.map_utils").cmd_map
local plug_map = require("me.map_utils").plug_map

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
cmd_map{keys = "ZE", command = 'Telescope find_files previewer=false prompt_prefix=🔍\\ '}
cmd_map{keys = "ZD", command = "BufferWipeout"}
-- Save the file only when the buffer has been modified.
cmd_map{keys = "ZW", command = "update"}
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
cmd_map{keys = "ZR", command = "TroubleToggle"}

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
map{keys = "<leader>gY", to = "<cmd>lua require('gitlinker').get_repo_url()<cr>"}
map{keys = "<leader>gB", to = "<cmd>lua require('gitlinker').get_repo_url({action_callback = require('gitlinker.actions').open_in_browser})<cr>"}
map{keys = "<leader>gy", to = "<cmd>lua require('gitlinker').get_buf_range_url('n')<cr>"}
map{keys = "<leader>gy", to = "<cmd>lua require('gitlinker').get_buf_range_url('v')<cr>", mode = "v"}


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
