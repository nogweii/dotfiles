local javascript_fts = {
  'javascript',
  'javascriptreact',
  'javascript.jsx',
  'typescript',
  'typescriptreact',
  'typescript.tsx',
}

---@type LazySpec
return {
  {
    'pmizio/typescript-tools.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'neovim/nvim-lspconfig',
      {
        'marilari88/twoslash-queries.nvim',
        opts = { multi_line = true },
      },
    },
    opts = {
      on_attach = function(client, bufnr)
        require('twoslash-queries').attach(client, bufnr)
      end,
    },
    ft = javascript_fts,
  },

  {
    'dmmulroy/ts-error-translator.nvim',
    opts = {
      auto_override_publish_diagnostics = true,
    },
  },

  {
    'dmmulroy/tsc.nvim',
    ft = javascript_fts,
    opts = {
      auto_override_publish_diagnostics = true,
    },
  },
}
