local add_hook_after = require('lspconfig.util').add_hook_after
local config = require('me.settings.config')
require("mason").setup()
require("mason-lspconfig").setup()

-- Additional language servers:
--    termux-language-server
local configs = require 'lspconfig.configs'
if not configs['termux_ls'] then
  configs['termux_ls'] = require 'lspconfig/configs/termux_ls'
end
local mason_lspconfig = (require 'mason-lspconfig').get_mappings()
mason_lspconfig.lspconfig_to_package['termux_ls'] = 'termux-language-server'
mason_lspconfig.package_to_lspconfig['termux-language-server'] = 'termux_ls'

vim.diagnostic.config({
  underline = true,
  update_in_insert = false,
  virtual_text = {
    spacing = 4,
    source = 'if_many',
  },
  severity_sort = true,
})

vim.diagnostic.config({
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = '',
      [vim.diagnostic.severity.WARN] = '',
      [vim.diagnostic.severity.HINT] = '',
      [vim.diagnostic.severity.INFO] = '',
    },
  },
})

-- do stuff after any LSP server attaches to the document
local function any_lsp_attach(client, bufnr)
  local bufopt = vim.bo[bufnr]
  bufopt.omnifunc = 'v:lua.vim.lsp.omnifunc'
  bufopt.tagfunc = 'v:lua.vim.lsp.tagfunc'
  bufopt.formatexpr = 'v:lua.vim.lsp.formatexpr'

  -- add the LSP's determined root directory to &path
  if client.workspace_folders ~= nil then
    vim.opt_local.path:append(vim.tbl_map(function(folder)
      return folder.name
    end, client.workspace_folders))
  end

  -- connect the lsp-format autocommand to the buffer
  require('lsp-format').on_attach(client, bufnr)
end

local make_capabilities = function()
  local capabilities = vim.lsp.protocol.make_client_capabilities()

  -- nvim-ufo knows how to handle LSP defined fold ranges
  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
  }

  -- I've set up LuaSnip, which knows how to parse LSP provided snippets
  capabilities.textDocument.completion.completionItem.snippetSupport = true

  local cmp_lsp_caps = require('cmp_nvim_lsp').default_capabilities()
  vim.tbl_deep_extend('force', capabilities, cmp_lsp_caps)

  return capabilities
end

local function setup_lsp_server(name)
  local opts = {
    -- map buffer local keybindings when the language server attaches
    capabilities = make_capabilities(),
  }

  local has_lsp_config, custom_lsp_config = pcall(require, 'me.settings.lsp_servers.' .. name)
  if has_lsp_config then
    opts = vim.tbl_deep_extend('force', opts, custom_lsp_config)
  end

  -- A server config file can optionally define custom on_attach handlers
  opts.on_attach = add_hook_after(opts.on_attach, any_lsp_attach)

  -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/configs.md
  local upstream_config = require('lspconfig')[name]
  if upstream_config.setup then
    upstream_config.setup(opts)
  else
    vim.notify('No such LSP config ' .. name, vim.log.levels.DEBUG)
  end
end

require("mason-lspconfig").setup_handlers {
  -- The first entry (without a key) will be the default handler
  -- and will be called for each installed server that doesn't have
  -- a dedicated handler.
  function(server_name)
    if vim.tbl_contains(config.disabled_lsp, server_name) then
      return {}
    end
    setup_lsp_server(server_name)
  end,

  -- Next, you can provide a dedicated handler for specific servers.
  -- For example, a handler override for the `rust_analyzer`:
  -- ["rust_analyzer"] = function ()
  --     require("rust-tools").setup {}
  -- end
}
