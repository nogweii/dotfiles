require('neodev').setup({})
local add_hook_after = require('lspconfig.util').add_hook_after

-- keymaps
local function on_attach(_client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap = true, silent = true }
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', '<C-]>', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
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

-- A list of binaries found in $PATH and what configuration
-- that powers
local binaries_to_lsp = {
  { 'bash-language-server', 'bashls' },
  { 'vscode-html-languageserver', 'html' },
  { 'vscode-css-languageserver', 'cssls' },
  { 'vscode-json-languageserver', 'jsonls' },
  { 'vscode-html-language-server', 'html' },
  { 'vscode-css-language-server', 'cssls' },
  { 'vscode-json-language-server', 'jsonls' },
  { 'yaml-language-server', 'yamlls' },
  { 'lua-language-server', 'lua_ls' },
  { 'typescript-language-server', 'tsserver' },
  { 'gopls', 'gopls' },
  { 'zk', 'zk' },
  { 'texlab', 'texlab' },
  { 'ccls', 'ccls' },
  { 'dhall-lsp-server', 'dhall_lsp_server' },
  { 'haskell-language-server-wrapper', 'hls' },
  { 'pylsp', 'pylsp' },
  { 'ansible-language-server', 'ansiblels' },
  { 'terraform-ls', 'terraformls' },
  { 'marksman', 'marksman' },
  { 'docker-langserver', 'dockerls' },
  { 'tflint', 'tflint' },
  { 'jsonnet-language-server', 'jsonnet_ls' },
}

for _, lsp_map in pairs(binaries_to_lsp) do
  if vim.fn.executable(lsp_map[1]) == 1 then
    setup_lsp_server(lsp_map[2])
  end
end
