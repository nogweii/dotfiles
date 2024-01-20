-- Various additional filetype definitions

vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufReadPost', 'FileReadPost' }, {
  pattern = { 'gemrc' },
  callback = function()
    vim.opt_local.filetype = 'yaml'
  end,
})
