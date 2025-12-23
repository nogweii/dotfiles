--#region Configure diagnostics
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
--#endregion

--#region Per-buffer tweaks once an LSP attaches
vim.api.nvim_create_autocmd({ 'LspAttach' }, {
  callback = function(args)
    local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
    local bufnr = args.buf

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
  end,
})
--#endregion

--#region Additional LSP capabilities
local capabilities = {
  textDocument = {
    -- nvim-ufo knows how to handle LSP defined fold ranges
    foldingRange = {
      dynamicRegistration = false,
      lineFoldingOnly = true,
    },

    -- I've set up LuaSnip, which knows how to parse LSP provided snippets
    completion = {
      completionItem = {
        snippetSupport = true
      }
    }
  }
}

local cmp_lsp_caps = require('cmp_nvim_lsp').default_capabilities()
vim.tbl_deep_extend('force', capabilities, cmp_lsp_caps)
--#endregion

-- Special configuration for all LSP servers
vim.lsp.config('*', {
  capabilities = capabilities,
})

require("mason").setup()
require("mason-lspconfig").setup()
