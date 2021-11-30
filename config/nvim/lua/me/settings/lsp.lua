local lspconfig = require('lspconfig')
local lspconfigs = require('lspconfig.configs')
local lspkind = require('lspkind')
local lsp_installer = require("nvim-lsp-installer")
local coq = require("coq")

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

lsp_installer.on_server_ready(function(server)
  local opts = {
    -- map buffer local keybindings when the language server attaches
    on_attach = M.on_attach,
  }

  local has_lsp_config, custom_lsp_config = pcall(require, "me.settings.lsp_configs." .. server.name)
  if has_lsp_config then
    opts = vim.tbl_deep_extend("force", opts, custom_lsp_config)
  end

  -- This setup() function is exactly the same as lspconfig's setup function.
  -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
  server:setup(coq.lsp_ensure_capabilities(opts))
end)

return M
