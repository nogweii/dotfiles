-- NeoVim editor options.
-- Looking for configuration for plugins? Check settings/* and settings.lua

vim.opt.showmode = false -- Hide the mode indication in the command bar, since my status bar has it

vim.opt.wrap = false                     -- Don't wrap lines
vim.opt.errorbells = false               -- Disable any error bells
vim.opt.laststatus = 2               -- Always show the status bar
vim.opt.hidden = true                     -- Allow changing buffers even with modifications
vim.opt.spell = true                      -- Enable spell check
vim.opt.spellcapcheck = ''             -- Disable capitalization spell check
vim.opt.title = true                      -- Modify the terminal title
vim.opt.hlsearch = true                   -- Highlight search results
vim.opt.incsearch = true                  -- Jump to the first match in real-time
vim.opt.ignorecase = true                 -- Case insensitive search, by default.
vim.opt.smartcase = true                  -- Case-sensitive if there any capital letters
vim.opt.modeline = true                   -- Let individual files specify settings
vim.opt.background = "dark"            -- Use dark colors over lighter ones
vim.opt.fileformat= "unix"            -- Prefer UNIX line endings
vim.opt.history = 10000              -- Remember lots of history for :
vim.opt.wildmenu = true                   -- Show completion matches in the status bar
vim.opt.wildmode = "longest,full"      -- Expand the longest common match, then all
vim.opt.backspace= "indent,eol,start" -- Smart backspace in insert mode
vim.opt.sidescroll=1               -- Scroll horizontally 1 column at a time
vim.opt.sidescrolloff=7            -- Always show this at least this many columns
vim.opt.fileencoding = "utf-8"         -- Default to assuming files are encoded in UTF-8
vim.opt.updatetime=100             -- Millisecs idle before calling the CursorHold

vim.opt.complete = vim.opt.complete + 'kspell'         -- Scan spell checker's dictionary for completion as well
vim.opt.completeopt = "noinsert,menuone,noselect,preview"
vim.opt.virtualedit = "block"         -- Block movement can go beyond end-of-line
vim.opt.modelines = 3                -- Search the top and bottom 3 lines for modelines
vim.opt.number = true                     -- Show line numbers
vim.opt.undofile = true                   -- Persist undo history across sessions
vim.opt.termguicolors = true              -- Use guifg over ctermfg in true-color terminals
vim.opt.conceallevel = 2              -- Automatically conceal characters

vim.opt.sessionoptions = vim.opt.sessionoptions
              - "blank"        -- Don't save empty windows in the session
              - "buffers"      -- Don't save hidden buffers into the session
              - "help"         -- Ignore the help buffer for sessions
              - "options"      -- Don't save any vim options (this list)
              - "globals"      -- Ignore any g:-variables
              + "localoptions" -- Include buffer local overrides
              + "tabpages"     -- This session is for all tabs, not individual ones

vim.opt.formatoptions = vim.opt.formatoptions
                 + "r"         -- Add comment syntax to new lines in insert mode
                 + "o"         -- Automatically add comment syntax after o/O

vim.opt.shortmess = vim.o.shortmess
                .. "F" -- Don't print a message when opening a file
                .. "c" -- silence insert mode completion messages

vim.opt.foldlevel = 5              -- Only fold sections deeper than this level automatically
vim.opt.foldlevelstart = 5         -- Only fold sections deeper than this level automatically


-- ignore a bunch of stuff in the wildmenu completion
vim.opt.wildignore = {
  ".DS_Store", ".git", ".svn", ".hg",
  "*.a", "*.o", "*.obj", "*.out",
  "*.so", "*.dll", "*.exe", "*.bin",
  "*~", "*.swp", "*.tmp",
  "*.bmp", "*.gif", "*.ico", "*.jpg", "*.jpeg", "*.png",
  "__pycache__", "*.pyc", "*pycache*",
  "*.tar", "*.gz", "*.bz2", "*.zstd", "*.xz", "*.zip",
  '*.ttf', '*.otf', '*.woff', '*.woff2', '*.eot'
}


-- Point the spell checker at my additional vocabulary words
vim.opt.spellfile = vim.fn.stdpath("config") .. "/spell/en.utf-8.add"

-- Calculate the offset required to the cursor vertically centered on-screen.
vim.opt.scrolloff = vim.o.lines - 2

vim.opt.backupdir = vim.fn.stdpath("data") .. '/backup'
if not vim.fn.isdirectory(vim.o.backupdir) then
  vim.fn.mkdir(vim.o.backupdir, "p")
end

-- Don't always show the sign columns, but if there are, make sure there's room
-- for two. This matches the width of the mode indicator in the statusbar
vim.opt.signcolumn = 'auto:2'

-- Add angle brackets to the list of recognized characters in a pair
vim.opt.matchpairs = vim.bo.matchpairs .. ",<:>"
