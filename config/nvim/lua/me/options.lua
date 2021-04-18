-- NeoVim editor options

-- pull in @tjdevries's lovely wrapper to make setting neovim options ergonomic
-- unneeded once https://github.com/neovim/neovim/pull/13479 is merged
require('tj.opt')
local opt = vim.opt

opt.showmode = false -- Hide the mode indication in the command bar, since my status bar has it

opt.wrap = false                     -- Don't wrap lines
opt.errorbells = false               -- Disable any error bells
opt.laststatus = 2               -- Always show the status bar
opt.hidden = true                     -- Allow changing buffers even with modifications
opt.spell = true                      -- Enable spell check
opt.spellcapcheck = ''             -- Disable capitalization spell check
opt.title = true                      -- Modify the terminal title
opt.hlsearch = true                   -- Highlight search results
opt.incsearch = true                  -- Jump to the first match in real-time
opt.ignorecase = true                 -- Case insensitive search, by default.
opt.smartcase = true                  -- Case-sensitive if there any capital letters
opt.modeline = true                   -- Let individual files specify settings
opt.background = "dark"            -- Use dark colors over lighter ones
opt.fileformat= "unix"            -- Prefer UNIX line endings
opt.history = 10000              -- Remember lots of history for :
opt.wildmenu = true                   -- Show completion matches in the status bar
opt.wildmode = "longest,full"      -- Expand the longest common match, then all
opt.backspace= "indent,eol,start" -- Smart backspace in insert mode
opt.sidescroll=1               -- Scroll horizontally 1 column at a time
opt.sidescrolloff=7            -- Always show this at least this many columns
opt.fileencoding = "utf-8"         -- Default to assuming files are encoded in UTF-8
opt.updatetime=100             -- Millisecs idle before calling the CursorHold

opt.complete = opt.complete + 'kspell'         -- Scan spell checker's dictionary for completion as well
opt.completeopt = "noinsert,menuone,noselect,preview"
opt.virtualedit = "block"         -- Block movement can go beyond end-of-line
opt.modelines = 3                -- Search the top and bottom 3 lines for modelines
opt.number = true                     -- Show line numbers
opt.undofile = true                   -- Persist undo history across sessions
opt.termguicolors = true              -- Use guifg over ctermfg in true-color terminals

opt.sessionoptions = opt.sessionoptions
              - "blank"        -- Don't save empty windows in the session
              - "buffers"      -- Don't save hidden buffers into the session
              - "help"         -- Ignore the help buffer for sessions
              - "options"      -- Don't save any vim options (this list)
              - "globals"      -- Ignore any g:-variables
              + "localoptions" -- Include buffer local overrides
              + "tabpages"     -- This session is for all tabs, not individual ones

opt.formatoptions = opt.formatoptions
                 + "r"         -- Add comment syntax to new lines in insert mode
                 + "o"         -- Automatically add comment syntax after o/O

opt.shortmess = vim.o.shortmess
                .. "F" -- Don't print a message when opening a file
                .. "c" -- silence insert mode completion messages

opt.foldlevel = 5              -- Only fold sections deeper than this level automatically
opt.foldlevelstart = 5         -- Only fold sections deeper than this level automatically


-- ignore a bunch of stuff in the wildmenu completion
opt.wildignore = {
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
opt.spellfile = vim.fn.stdpath("config") .. "/en.utf-8.add"

-- Calculate the offset required to the cursor vertically centered on-screen.
opt.scrolloff = vim.o.lines - 2

-- My default indentation settings. Plugins will scan the buffer for the
-- correct settings, but these are my preferred fallback:
opt.softtabstop = 2
opt.shiftwidth = 2
opt.tabstop = 4
opt.expandtab = true

opt.backupdir = vim.fn.stdpath("data") .. '/backup'
if not vim.fn.isdirectory(vim.o.backupdir) then
  vim.fn.mkdir(vim.o.backupdir, "p")
end
