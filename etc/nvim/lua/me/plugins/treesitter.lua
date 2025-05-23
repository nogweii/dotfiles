---@type LazySpec
return {

  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    version = false,
    config = function()
      ---@diagnostic disable-next-line: missing-fields
      require('nvim-treesitter.configs').setup({
        -- Enable some modules shipped with nvim-treesitter
        highlight = {
          enable = true,
        },
        indent = {
          enable = true,
        },
      })
    end,
    event = 'AsyncFileLoad',
  },

  -- Treesitter compatible rainbow parentheses
  {
    url = 'https://gitlab.com/HiPhish/rainbow-delimiters.nvim',
    name = 'rainbow-delimiters.nvim',
    submodules = false,
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      -- This module contains a number of default definitions
      local rainbow_delimiters = require('rainbow-delimiters')

      ---@type rainbow_delimiters.config
      vim.g.rainbow_delimiters = {
        strategy = {
          [''] = rainbow_delimiters.strategy['global'],
        },
        -- see |rb-delimiters-query| for more details on these
        query = {
          [''] = 'rainbow-delimiters',
          lua = 'rainbow-blocks',
          latex = 'rainbow-blocks',
          javascript = 'rainbow-parens',
          typescript = 'rainbow-parens',
        },
        --[[ highlight = {
              'RainbowDelimiterRed',
              'RainbowDelimiterYellow',
              'RainbowDelimiterBlue',
              'RainbowDelimiterOrange',
              'RainbowDelimiterGreen',
              'RainbowDelimiterViolet',
              'RainbowDelimiterCyan',
          }, ]]
      }
    end,
    event = 'AsyncFileLoad',
  },

  -- Dynamically set &commentstring when moving around files with multiple filetypes combined
  {
    'JoosepAlviste/nvim-ts-context-commentstring',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    opts = {},
    event = 'AsyncFileLoad',
  },

  -- Add some context to where I am in a file
  {
    'nvim-treesitter/nvim-treesitter-context',
    opts = {
      max_lines = 2,
    },
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    event = 'AsyncFileLoad',
  },

  {
    'windwp/nvim-ts-autotag',
    opts = {},
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    event = 'AsyncFileLoad',
  },

  {
    'RRethy/nvim-treesitter-endwise',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('nvim-treesitter-endwise').init()
    end,
    event = 'AsyncFileLoad',
  },

  -- Use custom treesitter queries to highlight function arguments
  {
    'm-demare/hlargs.nvim',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    event = 'AsyncFileLoad',
  },
}
