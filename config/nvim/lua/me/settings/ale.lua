vim.g.ale_enabled = 1

-- don't use the custom LSP client in ALE, I'm using the native one in NeoVim
-- nvim-ale-diagnostic is what glues the two together
vim.g.ale_disable_lsp = 1

-- prettier signs
vim.g.ale_sign_error = ""
vim.g.ale_sign_warning = ""
vim.g.ale_sign_info = ""
vim.g.ale_sign_style_error = ""
vim.g.ale_sign_style_warning = ""

-- use a floating window to show the lint problems
vim.g.ale_floating_preview = 1
vim.g.ale_floating_window_border = {'│', '─', '╭', '╮', '╯', '╰'}
vim.g.ale_hover_cursor = 1

-- don't run the linter(s) for this file when I edit text, live, instead only
-- do so when I leave insert mode
vim.g.ale_lint_on_text_changed = 'never'
vim.g.ale_lint_on_insert_leave = 1

-- echo a message whenever my cursor moves to a line with a problem
vim.g.ale_echo_cursor = 1
-- if there's an ALE preview window open, close it when I enter insert mode
vim.g.ale_close_preview_on_insert = 1

-- customize the linters
vim.g.ale_yaml_yamllint_options = '-f ' .. vim.fn.stdpath("config") .. "/linters/yamllint.yml"
