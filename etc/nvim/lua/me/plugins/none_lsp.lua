---@type LazySpec[]
return {
  -- a collection of LSP configs
  {
    'nvimtools/none-ls.nvim',
    dependencies = {
      'nvimtools/none-ls-extras.nvim',
    },
    main = 'null-ls',
    opts = function(_self, opts)
      local nls = require('null-ls')
      opts.root_dir = opts.root_dir or require('null-ls.utils').root_pattern('.null-ls-root', '.nvim.lua', '.git')
      opts.sources = {
        nls.builtins.formatting.stylua,
        nls.builtins.completion.spell,
        nls.builtins.formatting.stylua,
        nls.builtins.formatting.shfmt,
        -- these are from none-ls-extras.nvim
        require('none-ls.diagnostics.eslint'),
      }
    end,
  },
}
