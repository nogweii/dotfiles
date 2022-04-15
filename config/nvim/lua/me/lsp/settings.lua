local lspconfig = require('lspconfig')
local lspconfigs = require('lspconfig.configs')
local lsp_installer = require("nvim-lsp-installer")

local M = {}

-- keymaps
function M.on_attach(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', '<C-]>', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)

  -- Not all LSPs implement every bit of functionality; check the particular
  -- LSP server we've attached to for these

  if (client.resolved_capabilities.document_formatting or client.resolved_capabilities.document_range_formatting) then
    buf_set_option('formatexpr', 'v:lua.vim.lsp.formatexpr(#{timeout_ms:250})')
  end
end

local make_capabilities = function()
  local capabilities = vim.lsp.protocol.make_client_capabilities()

  -- TODO: everyone does this, but what does it enable?
  capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
  capabilities.textDocument.completion.completionItem.preselectSupport = true

  -- I've set up LuaSnip, which knows how to parse LSP provided snippets
  capabilities.textDocument.completion.completionItem.snippetSupport = true

  capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

  return capabilities
end

lsp_installer.on_server_ready(function(server)
  local opts = {
    -- map buffer local keybindings when the language server attaches
    on_attach = M.on_attach,
    capabilities = make_capabilities()
  }

  local has_lsp_config, custom_lsp_config = pcall(require, "me.lsp.configs." .. server.name)
  if has_lsp_config then
    opts = vim.tbl_deep_extend("force", opts, custom_lsp_config)
  end

  -- This setup() function is exactly the same as lspconfig's setup function.
  -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
  server:setup(opts)
end)

return M
