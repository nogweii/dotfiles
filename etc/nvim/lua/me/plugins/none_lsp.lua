---@type LazySpec[]
return {
  -- a collection of LSP configs
  {
    'nvimtools/none-ls.nvim',
    event = 'AsyncFileLoad',
    dependencies = {
      'nvimtools/none-ls-extras.nvim',
      {
        'ckolkey/ts-node-action',
        dependencies = { 'nvim-treesitter' },
        opts = {},
      },
      {
        'ThePrimeagen/refactoring.nvim',
        dependencies = {
          'nvim-lua/plenary.nvim',
          'nvim-treesitter/nvim-treesitter',
        },
        ---@type ConfigOpts
        opts = {
          show_success_message = true,
        }
      },
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
        nls.builtins.formatting.shellharden,
        nls.builtins.code_actions.gitrebase,
        nls.builtins.code_actions.gitsigns,
        nls.builtins.code_actions.ts_node_action,
        nls.builtins.code_actions.refactoring,
        nls.builtins.hover.dictionary,
        nls.builtins.hover.printenv,
        nls.builtins.diagnostics.zsh,
        nls.builtins.diagnostics.terraform_validate,
        nls.builtins.diagnostics.editorconfig_checker,
        nls.builtins.diagnostics.dotenv_linter,
        nls.builtins.formatting.black,
        nls.builtins.formatting.mix,
        nls.builtins.formatting.gofmt,
        nls.builtins.formatting.terraform_fmt,
        nls.builtins.formatting.yamlfmt,
        nls.builtins.formatting.packer,
        nls.builtins.diagnostics.hadolint,
        nls.builtins.diagnostics.stylelint,

        -- these are from none-ls-extras.nvim
        require('none-ls.diagnostics.eslint'),
        require('none-ls.diagnostics.yamllint'),
        require('none-ls.diagnostics.cpplint'),
        require('none-ls.formatting.jq'),
        require('none-ls.formatting.standardrb'),

        -- and here are some of my own custom additions
        require('me.none_ls.formatters.formatjson5'),
        require('me.none_ls.diagnostics.typos'),
      }
    end,
  },
}
