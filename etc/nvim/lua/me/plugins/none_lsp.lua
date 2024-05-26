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
        nls.builtins.code_actions.gitrebase,
        nls.builtins.code_actions.gitsigns,
        nls.builtins.code_actions.refactoring,
        nls.builtins.code_actions.ts_node_action,
        nls.builtins.completion.spell,
        nls.builtins.diagnostics.dotenv_linter,
        nls.builtins.diagnostics.terraform_validate,
        nls.builtins.diagnostics.zsh,
        nls.builtins.formatting.packer,
        nls.builtins.formatting.terraform_fmt,
        nls.builtins.hover.dictionary,
        nls.builtins.hover.printenv,

        nls.builtins.diagnostics.markdownlint.with({
          extra_args = function(_params)
            local utils = require("me.utils")
            local fallback_conf = utils.alternative_config_file(
              { ".markdownlint.jsonc", ".markdownlint.json", ".markdownlint.yaml", ".markdownlint.yml" },
              "markdownlint.jsonc"
            )

            if fallback_conf then
              return { '--config', fallback_conf }
            end

            return {}
          end
        }),

        -- these are from none-ls-extras.nvim
        require('none-ls.diagnostics.eslint_d'),
        require('none-ls.diagnostics.yamllint').with({
          extra_args = function(_params)
            local utils = require("me.utils")
            local fallback_conf = utils.alternative_config_file(
              { ".yamllint", ".yamllint.yaml", ".yamllint.yml" },
              "yamllint.yaml"
            )

            if fallback_conf then
              return { '--config-file', fallback_conf }
            end

            return {}
          end
        }),
        require('none-ls.diagnostics.cpplint'),
        require('none-ls.formatting.jq'),

        -- and here are some of my own custom additions
        require('me.none_ls.formatters.formatjson5'),
        -- require('me.none_ls.diagnostics.typos'),
      }
    end,
  },
}
