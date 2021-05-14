-- Set various global variables to configure all of the plugins I've installed

if vim.fn.executable('editorconfig') then
 vim.g.EditorConfig_core_mode = "external_command"
 vim.g.EditorConfig_exec_path = vim.fn.exepath('editorconfig')
end

-- Switch to the file's current directory if we're not in a found project
vim.g.rooter_change_directory_for_non_project_files = 'current'
-- Change only the local buffer's directory, not the entire vim
vim.g.rooter_cd_cmd = 'lcd'
-- Be silent when changing directories
vim.g.rooter_silent_chdir = 1
-- NOTE: rooter sets `b:rootDir` to the absolute path of the folder

vim.api.nvim_set_var("test#strategy", "mine")

-- Switch sandwich to using surround.vim's key bindings, which I'm very used
-- to, while still taking advantage of the extra functionality
vim.cmd [[runtime macros/sandwich/keymap/surround.vim]]

vim.g.nvim_tree_quit_on_open = 1 -- After selecting a file in tree, close it
vim.g.nvim_tree_add_trailing = 1 -- Trailing slashes on directories
vim.g.nvim_tree_follow = 1

-- Speed up loading vim remote plugin hosts. Since they do a more complicated
-- heuristic than my systems need, I can shortcut the work needed.
if vim.fn.executable('neovim-ruby-host') then
  vim.g.ruby_host_prog = vim.fn.exepath('neovim-ruby-host')
end
vim.g.python3_host_prog = '/usr/bin/python3'
vim.g.loaded_python_provider = 0

-- Disable all of UltiSnip's default mappings
vim.g.UltiSnipsExpandTrigger = "<NUL>"
vim.g.UltiSnipsListSnippets = "<NUL>"
vim.g.UltiSnipsJumpForwardTrigger = "<NUL>"
vim.g.UltiSnipsJumpBackwardTrigger = "<NUL>"

-- Gutentags configuration
vim.g.gutentags_enabled = 0 -- by default it's disabled
vim.g.gutentags_ctags_tagfile = ".vim-stuff/tags" -- TODO: auto-enable when this folder exists
vim.g.gutentags_ctags_executable_ruby = 'ripper-tags'

-- lastplace configuration
vim.g.lastplace_ignore_buftype = "quickfix,nofile,help"
vim.g.lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
vim.g.lastplace_open_folds = 1

-- Grepper configuration
vim.g.grepper = {
  open = 0,
  quickfix = 1,
  searchreg = 1,
  highlight = 0,
}
