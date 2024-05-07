require('neodev').setup({})
local add_hook_after = require('lspconfig.util').add_hook_after

vim.diagnostic.config({
  underline = true,
  update_in_insert = false,
  virtual_text = {
    spacing = 4,
    source = 'if_many',
  },
  severity_sort = true,
})

local diagnostics_signs = {
  [vim.diagnostic.severity.ERROR] = '',
  [vim.diagnostic.severity.WARN] = '',
  [vim.diagnostic.severity.HINT] = '',
  [vim.diagnostic.severity.INFO] = '',
}
for severity, icon in pairs(diagnostics_signs) do
  local name = vim.diagnostic.severity[severity]:lower():gsub('^%l', string.upper)
  name = 'DiagnosticSign' .. name
  vim.fn.sign_define(name, { text = icon, texthl = name, numhl = '' })
end

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
  opts.on_attach = add_hook_after(any_lsp_attach, opts.on_attach)

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
  'crystalline',
}

for i = 1, #lsps do
  setup_lsp_server(lsps[i])
end
