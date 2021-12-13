function nvim_create_augroups(definitions)
	for group_name, definition in pairs(definitions) do
		vim.cmd("augroup " .. group_name)
		vim.cmd("autocmd!")
		for _, def in ipairs(definition) do
			local command = table.concat(vim.tbl_flatten{"autocmd", def}, ' ')
			vim.cmd(command)
		end
		vim.cmd("augroup END")
	end
end

nvim_create_augroups({
  nospell_types = {
    {"FileType", "lua", "setlocal", "nospell"},
  },

  nvim_tree_changes = {
    {"FileType", "NvimTree", "setlocal", "cursorline"},
  },

  highlight_yank = { -- Copied from :help lua-highlight
    {"TextYankPost", "*", "silent! lua vim.highlight.on_yank {timeout=500}"},
  },

  quickfix_windows = {
    {'QuickFixCmdPost', 'grep', 'cwindow'},
    {'QuickFixCmdPost', 'helpgrep', 'cwindow'},
    {'FileType', 'qf', 'setlocal scrolloff=0 nobuflisted'},
  },

  grepper = {
    {'User', 'Grepper', [[call setqflist([], 'r', {'context': {'bqf': {'pattern_hl': histget('/')}}}) | botright copen]]},
  },

  postgresql_configs = {
    {'BufNewFile,BufRead', 'psqlrc,.psqlrc', [[let b:sql_type_override='pgsql' | setfiletype sql]]}
  },

  lsp_document_highlight = {
    {'CursorMoved', '<buffer>', [[lua vim.lsp.buf.clear_references()]]}
  },
})
