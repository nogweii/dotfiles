-- Various additional filetype definitions

vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufReadPost', 'FileReadPost' }, {
  pattern = { 'gemrc' },
  callback = function()
    vim.opt_local.filetype = 'yaml'
  end,
})

vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufReadPost', 'FileReadPost' }, {
  pattern = { 'Podfile', 'Fastfile' },
  callback = function()
    vim.opt_local.filetype = 'ruby'
  end,
})
