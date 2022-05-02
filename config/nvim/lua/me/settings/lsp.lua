local lsp_installer = require("nvim-lsp-installer")
local lspconfig = require('lspconfig')

-- keymaps
local function on_attach(client, bufnr)
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

local function setup_lsp_server(name)
  local opts = {
    -- map buffer local keybindings when the language server attaches
    on_attach = on_attach,
    capabilities = make_capabilities()
  }

  local has_lsp_config, custom_lsp_config = pcall(require, "me.settings.lsp_servers." .. name)
  if has_lsp_config then
    opts = vim.tbl_deep_extend("force", opts, custom_lsp_config)
  end

  -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
  lspconfig[name].setup(opts)
end

-- Configure & initialize LSP Installer
lsp_installer.setup({
    automatic_installation = false,
    ui = {
        icons = {
            server_installed = "💾",
            server_pending = "🎁",
            server_uninstalled = "❓"
        }
    }
})

-- Then register all of the servers that have been installed via LSP installer
for _, server in pairs(lsp_installer.get_installed_servers()) do
  setup_lsp_server(server.name)
end

local arch_package_binaries_to_lsp = {
  {"bash-language-server", "bashls"},
  {"vscode-html-languageserver", "html"},
  {"vscode-css-languageserver", "cssls"},
  {"vscode-json-languageserver", "jsonls"},
  {"yaml-language-server", "yamlls"},
  {"lua-language-server", "sumneko_lua"},
  {"typescript-language-server", "tsserver"},
  {"gopls", "gopls"},
  {"zk", "zk"},
  {"texlab", "texlab"},
  {"ccls", "ccls"},
  {"dhall-lsp-server", "dhall_lsp_server"},
  {"haskell-language-server-wrapper", "hls"},
  {"pylsp", "pylsp"},
}

for _, lsp_map in pairs(arch_package_binaries_to_lsp) do
  if vim.fn.executable(lsp_map[1]) == 1 then
    setup_lsp_server(lsp_map[2])
  end
end
