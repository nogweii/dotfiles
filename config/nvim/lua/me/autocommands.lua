function nvim_create_augroups(definitions)
	for group_name, definition in pairs(definitions) do
		vim.api.nvim_command("augroup " .. group_name)
		vim.api.nvim_command("autocmd!")
		for _, def in ipairs(definition) do
			local command = table.concat(vim.tbl_flatten{"autocmd", def}, ' ')
			vim.api.nvim_command(command)
		end
		vim.api.nvim_command("augroup END")
	end
end

nvim_create_augroups({
  nospell_types = {
    {"FileType", "lua", "setlocal", "nospell"},
  },

  nvim_tree_changes = {
    {"FileType", "NvimTree", "setlocal", "cursorline"},
  },

  highlight_yank = { -- Copied from :help	lua-highlight
    {"TextYankPost", "*", "silent! lua vim.highlight.on_yank()"},
  },

  restore_curpos = {
    {'BufReadPost', '*',
      [[
        if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
          exe "normal! g`\""
        endif
      ]],
    },
  },

  quickfix_windows = {
    {'QuickFixCmdPost', 'grep', 'cwindow'},
    {'QuickFixCmdPost', 'helpgrep', 'cwindow'},
    {'FileType', 'qf', 'setlocal scrolloff=0'},
  },
})
