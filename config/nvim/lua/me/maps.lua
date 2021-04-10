-- local vimp = require('vimp')

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
    })

    if type(args.to) == 'function' then
      -- a lua function was passed in, 
    end

    vim.api.nvim_set_keymap(args.mode, args.keys, args.to, { noremap = not args.recurse, silent = args.silent, expr = args.expression })
end

local function plug_map(args)
    map{keys = args.keys, to = '<Plug>(' .. args.command .. ')', mode = args.mode or 'n', silent = true, recurse = true}
end
local function cmd_map(args)
    map{keys = args.keys, to = '<cmd>' .. args.command .. '<CR>', silent = true, command = true}
end

-- Free up 'G' to be a generic prefix, and make gG do what G used to do
map{keys = "gG", to = "G"}
map{keys = "gG", to = "G", mode = "o"}
map{keys = "G", to = '', recurse = true}

-- nop-out semicolon, it's my mapleader key
map{keys = ";", to = '', recurse = true}
-- but restore the functionality by using the comma key
map{keys = ",", to = ";"}

-- Easily (un)indent again in visual mode by immediately re-selecting
map{keys = "<", to = "<gv", mode = "v"}
map{keys = ">", to = ">gv", mode = "v"}

plug_map{keys = "ZE", command = 'CommandT'}
plug_map{keys = "ZB", command = 'CommandTBuffer'}
-- Save the file only when the buffer has been modified.
cmd_map{keys = "ZD", command = "BufferWipeout"}
cmd_map{keys = "ZW", command = "update"}

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
cmd_map{keys = "-", command = "NvimTreeToggle"}

-- Git hunk jumps, that behave the same when diffing two files
map{keys = "]c", to = "&diff ? ']c' : '<cmd>lua require('gitsigns').next_hunk()<CR>'", expression = true}
map{keys = "[c", to = "&diff ? ']c' : '<cmd>lua require('gitsigns').prev_hunk()<CR>'", expression = true}

-- vimp.nnoremap('<leader>hw', function()
--   print('hello')
--   print('world')
-- end)
