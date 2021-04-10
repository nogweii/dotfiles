-- Set various global variables to configure all of the plugins I've installed

vim.cmd [[colorscheme zephyr]]

-- disable the base vim plugin from trying to autodetect the hexokinase binary,
-- since I'm managing it outside of the normal installation places
-- vim.g.loaded_hexokinase = 1
-- vim.g.Hexokinase_highlighters = {'virtual'}
-- if vim.fn.executable("hexokinase") then
--     vim.g.Hexokinase_executable_path = vim.fn.exepath("hexokinase")
--     vim.fn['hexokinase#v2#setup()']()
-- end

-- TODO: g:editorconfig_core_mode -> external_command
-- TODO: healthcheck for `editorconfig` from editorconfig-core-c package

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
