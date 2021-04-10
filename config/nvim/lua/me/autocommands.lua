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
})
