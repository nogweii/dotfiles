local lspconfig = require('lspconfig')
local lspconfigs = require('lspconfig.configs')
local lspcontainers = require('lspcontainers')
local lspkind = require('lspkind')
require("nvim-ale-diagnostic")

local M = {}

-- add some emoji decorations to the completion menu's suggestions
lspkind.init({
  with_text = true,
  symbol_map = {
    Text = '📜',
    Method = '🧶',
    Function = '🧵',
    Constructor = '🚧',
    Variable = '🔻',
    Class = '📦',
    Interface = '🧩',
    Module = '🚛',
    Property = '💊',
    Unit = '🗳 ',
    Value = '🧪',
    Enum = '🧫',
    Keyword = '🔑',
    Snippet = '🌱',
    Color = '🎨',
    File = '🗄 ',
    Folder = '📁',
    EnumMember = '🦠',
    Constant = '🧊',
    Struct = '🧱',
    Operator = '❎',
    Buffer = '🪐'
  },
})

-- forward diagnostics to ALE
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    underline = false,
    virtual_text = false,
    signs = true,
    update_in_insert = false,
  }
)

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

  -- TODO: switch when https://github.com/neovim/neovim/pull/13138 is merged
  -- buf_set_option('formatexpr', '')
  if client.resolved_capabilities.document_formatting then
    buf_set_keymap("n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  end
  if client.resolved_capabilities.document_range_formatting then
    buf_set_keymap("v", "<leader>f", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  end

  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
    augroup lsp_document_highlight
    autocmd! * <buffer>
    autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
    autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
    augroup END
    ]], false)
  end
end

-- config that activates keymaps and enables snippet support
function M.make_config(server_name)
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  -- capabilities.textDocument.completion.completionItem.snippetSupport = true
  local lsp_config = {
    -- enable snippet support
    capabilities = capabilities,
    -- map buffer local keybindings when the language server attaches
    on_attach = M.on_attach,
  }

  if vim.tbl_contains(lspcontainers.supported_languages, server_name) then
    lsp_config.cmd = lspcontainers.command(server_name)
    lsp_config.before_init = function(params)
      params.processId = vim.NIL
    end
  end

  return lsp_config
end

-- lsp-install
function M.setup_servers()
  for server, _ in pairs(lspcontainers.supported_languages) do
    lspconfig[server].setup(M.make_config(server))
  end
end

return M
