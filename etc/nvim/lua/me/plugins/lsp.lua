local toggle_formatting = function()
  local buf_id = vim.api.nvim_get_current_buf()
  local filetype = vim.bo[buf_id].filetype
  local formatting = require('lsp-format')

  if formatting.disabled_filetypes[filetype] then
    formatting.enable({ args = filetype, bang = false })
    vim.notify(filetype .. ' formatting turned on', vim.log.levels.INFO, { icon = '󰉿', render = 'compact' })
  else
    formatting.disable({ args = filetype, bang = false })
    vim.notify(filetype .. ' formatting turned off', vim.log.levels.INFO, { icon = '󰉥', render = 'compact' })
  end
end

---@type LazySpec[]
return {
  -- a collection of LSP configs
  {
    'neovim/nvim-lspconfig',
    config = function()
      require('me.settings.lsp')
    end,
    version = false, -- use latest commit rather than version
  },

  {
    'mrded/nvim-lsp-notify',
    dependencies = { 'rcarriga/nvim-notify' },
    event = 'LspAttach',
  },

  {
    'lukas-reineke/lsp-format.nvim',
    event = 'LspAttach',
    opts = {},
    keys = {
      { 'zF', toggle_formatting, desc = 'Toggle formatting' },
      { 'ZF', '<cmd>Format<CR>', desc = 'Format Buffer' },
    },
  },
}
