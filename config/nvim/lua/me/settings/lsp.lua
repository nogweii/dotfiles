local lspconfig = require('lspconfig')
local lspkind = require('lspkind')
local lspconfigs = require('lspconfig.configs')
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

-- Configure lua language server for neovim development
local neovim_lua_settings = {
  Lua = {
    runtime = {
      -- LuaJIT in the case of Neovim
      version = 'LuaJIT',
      path = vim.split(package.path, ';'),
    },
    diagnostics = {
      -- Get the language server to recognize the `vim` global
      globals = {'vim'},
    },
    workspace = {
      -- Make the server aware of Neovim runtime files
      library = {
        [vim.fn.expand('$VIMRUNTIME/lua')] = true,
        [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
      },
    },
  }
}

-- config that activates keymaps and enables snippet support
function M.make_config()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  -- capabilities.textDocument.completion.completionItem.snippetSupport = true
  return {
    -- enable snippet support
    capabilities = capabilities,
    -- map buffer local keybindings when the language server attaches
    on_attach = M.on_attach,
  }
end

-- lsp-install
function M.setup_servers()
  -- all known servers in containers
  local servers = { 'bashls', 'dockerls', 'gopls', 'rust_analyzer', 'sumneko_lua', 'tsserver', 'yamlls' }
  -- local LSP containers I'm building
  servers = vim.tbl_extend("force", servers, { 'pylsp' })

  for _, server in pairs(servers) do
    local config = M.make_config()

    -- language specific config
    -- if server == "lua" then
    --   TODO: check if the buffer is neovim related
    --   config.settings = neovim_lua_settings
    -- end

    local lsp_configuration = lspconfig[server]

    lsp_configuration.setup {
      before_init = function(params)
        params.processId = vim.NIL
      end,
      cmd = require('lspcontainers').command(server, {
        additional_languages = {
          pylsp = "lspcontainers/python-lsp:1.0.1"
        }
      }),
      root_dir = lspconfig.util.root_pattern(".git", vim.fn.getcwd()),
    }
  end
end

return M
