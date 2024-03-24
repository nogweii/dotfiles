vim.g.mapleader = ';'

local map = require('me.map_utils').map
local cmd_map = require('me.map_utils').cmd_map
local plug_map = require('me.map_utils').plug_map
local keymap = vim.keymap.set

--[[
                                 _
  __ _  ___ _ __   ___ _ __ __ _| |
 / _` |/ _ \ '_ \ / _ \ '__/ _` | |
| (_| |  __/ | | |  __/ | | (_| | |
 \__, |\___|_| |_|\___|_|  \__,_|_|
 |___/
 ]]
-- Free up 'G' to be a generic prefix, and make gG do what G used to do
map({ keys = 'gG', to = 'G', desc = 'Go to last line' })
map({ keys = 'gG', to = 'G', mode = 'o' })
map({ keys = 'G', to = '', recurse = true })

-- change Y to only select the line starting from the cursor
map({ keys = 'Y', to = 'y$' })

-- nop-out semicolon, it's my mapleader key
map({ keys = ';', to = '', recurse = true })
-- but restore the functionality by using the comma key
map({ keys = ',', to = ';' })

-- Easily (un)indent again in visual mode by immediately re-selecting
map({ keys = '<', to = '<gv', mode = 'v' })
map({ keys = '>', to = '>gv', mode = 'v' })

-- Sometimes you just need to move a character or two in insert mode. Don't
-- make these a habit, though!
map({ keys = '<C-j>', to = '<Down>', mode = 'i' })
map({ keys = '<C-k>', to = '<Up>', mode = 'i' })
map({ keys = '<C-h>', to = '<Left>', mode = 'i' })
map({ keys = '<C-l>', to = '<Right>', mode = 'i' })

-- Swap ` and ', making ' more precise (line & column) by default
map({ keys = '`', to = "'" })
map({ keys = "'", to = '`' })

-- Make interacting with the spell checking a little easier
keymap('n', 'zP', function()
  if vim.o.spell then
    vim.o.spell = false
    vim.notify('Spell checking disabled', vim.log.levels.INFO, { icon = '󰜺', render = 'compact' })
  else
    vim.o.spell = true
    vim.notify('Spell checking enabled', vim.log.levels.INFO, { icon = '󰓆', render = 'compact' })
  end
end, { desc = 'Toggle spell checking' })
map({ keys = 'zp', to = '1z=', desc = 'Accept the first spell correction' })

-- Attempt to clear the screen of artifacts and clear search highlight
map({ keys = '<C-l>', to = '<c-l>:nohlsearch<CR>:redraw<CR>' })

-- Map H and L to jump to the beginning or end of the line. H is smarter, in
-- that it jumps between the first non-whitespace character or actual column 0.
map({ keys = 'H', to = "(col('.') == matchend(getline('.'), '^\\s*')+1 ? '0' : '^')", expression = true })
map({ keys = 'L', to = '$', recurse = true })

-- Easily get out of insert mode in the terminal
map({ keys = '<C-s>', to = '<C-\\><C-n>', recurse = true, mode = 't', desc = 'Get out of insert mode' })

cmd_map({
  keys = 'ZE',
  command = 'Telescope find_files previewer=false prompt_prefix=🔍\\ ',
  desc = 'Fuzzily open a file',
})
cmd_map({ keys = 'ZW', command = 'update', desc = 'Save file changes' })
cmd_map({ keys = 'ZG', command = 'Telescope live_grep', desc = 'Search for text in all files' })
cmd_map({ keys = 'ZU', command = 'UndotreeToggle', desc = 'Visualize undo tree' })
cmd_map({ keys = 'ZR', command = 'TroubleToggle', desc = 'Show all LSP diagnostics' })
vim.keymap.set('n', 'Z-', function()
  require('telescope').extensions.projects.projects({})
end, { desc = 'Select project' })

-- Tap - to jump into a file pane
cmd_map({ keys = '-', command = 'Neotree filesystem reveal current' })

--[[
       _ _
  __ _(_) |_
 / _` | | __|
| (_| | | |_
 \__, |_|\__|
 |___/
 ]]
-- Git hunk jumps, that behave the same when diffing two files
map({ keys = ']c', to = "&diff ? ']c' : '<cmd>lua require('gitsigns').next_hunk()<CR>'", expression = true })
map({ keys = '[c', to = "&diff ? ']c' : '<cmd>lua require('gitsigns').prev_hunk()<CR>'", expression = true })
-- a motion to select the whole hunk
map({ mode = 'o', keys = 'ih', to = "<cmd>lua require('gitsigns').select_hunk()<CR>" })
map({ mode = 'x', keys = 'ih', to = "<cmd>lua require('gitsigns').select_hunk()<CR>" })
cmd_map({ keys = '<leader>gb', command = 'GitMessenger', desc = 'View history of current line' })
map({ keys = '<leader>gS', to = "<cmd>lua require('gitsigns').stage_hunk()<CR>", desc = 'Stage the hunk' })
map({ keys = '<leader>gU', to = "<cmd>lua require('gitsigns').undo_stage_hunk()<CR>", desc = 'Unstage the hunk' })
map({ keys = '<leader>gp', to = "<cmd>lua require('gitsigns').preview_hunk()<CR>", desc = 'Show the diff of the hunk' })
map({
  keys = '<leader>gB',
  to = "<cmd>lua require('gitlinker').get_repo_url({action_callback = require('gitlinker.actions').open_in_browser})<cr>",
  desc = 'Open the git repo in a browser',
})
