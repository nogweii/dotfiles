require('neodev').setup({})
local add_hook_after = require('lspconfig.util').add_hook_after

-- do stuff after the LSP server attaches to the document
local function on_attach(_client, bufnr)
  vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'
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
  opts.on_attach = add_hook_after(on_attach, opts.on_attach)

  -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
  require('lspconfig')[name].setup(opts)
end

local lsps = {
  'ansiblels',
  'bashls',
  'ccls',
  'cssls',
  'dhall_lsp_server',
  'dockerls',
  'gopls',
  'hls',
  'html',
  'jsonls',
  'jsonnet_ls',
  'lua_ls',
  'marksman',
  'pylsp',
  'terraformls',
  'texlab',
  'tflint',
  'yamlls',
  'zk',
}

for i = 1, #lsps do
  setup_lsp_server(lsps[i])
end
