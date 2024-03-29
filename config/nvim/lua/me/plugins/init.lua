---@type LazySpec
return {
  -- Easily put a character/pair around some text. Sandwich a word between
  -- parentheses!
  {
    'machakann/vim-sandwich',
    config = function()
      vim.cmd('runtime macros/sandwich/keymap/surround.vim')
    end,
  },

  {
    'ribru17/bamboo.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('bamboo').setup({})
      require('bamboo').load()
    end,
  },

  { 'stevearc/resession.nvim', opts = {} },

  -- Use :StartupTime to get an average of 10 runs of `nvim --startuptime` and
  -- present a nice display of what's taking so long startup. Also, see the shell
  -- alias 'nvim-startup-benchmark'
  {
    'dstein64/vim-startuptime',
    cmd = 'StartupTime',
    init = function()
      vim.g.startuptime_tries = 10
    end,
  },

  { 'tpope/vim-characterize' },
  { 'tpope/vim-rsi' },

  {
    'folke/trouble.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require('trouble').setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      })
    end,
  },

  -- smart <C-a> and <C-x> that knows how to change dates, enumerated strings, and regular numbers
  {
    'monaqa/dial.nvim',
    keys = {
      {
        '<C-a>',
        '<Plug>(dial-increment)',
        mode = { 'n', 'v' },
        desc = 'Increment or cycle the word under the cursor, smartly',
      },
      {
        '<C-x>',
        '<Plug>(dial-decrement)',
        mode = { 'n', 'v' },
        desc = 'Decrement or cycle the word under the cursor, smartly',
      },
      {
        'g<C-a>',
        'g<Plug>(dial-increment)',
        mode = { 'n', 'v' },
        desc = 'Increment or cycle the word under the cursor, smartly',
      },
      {
        'g<C-x>',
        'g<Plug>(dial-decrement)',
        mode = { 'n', 'v' },
        desc = 'Decrement or cycle the word under the cursor, smartly',
      },
    },
    config = function()
      require('me.settings.dial_swaps')
    end,
  },

  -- Automatic semi-smart indentation settings for a buffer
  {
    'nmac427/guess-indent.nvim',
    opts = {},
  },

  { 'mfussenegger/nvim-lint' },

  -- A smarter cursor position restoration function, excluding various buffers
  -- where it makes sense, and opening folds if needed.
  {
    'farmergreg/vim-lastplace',
    init = function()
      vim.g.lastplace_ignore_buftype = 'quickfix,nofile,help'
      vim.g.lastplace_ignore = 'gitcommit,gitrebase,svn,hgcommit'
      vim.g.lastplace_open_folds = 1
    end,
  },

  -- quickly & easily generate a python docstring
  {
    'heavenshell/vim-pydocstring',
    build = 'make install',
    ft = 'python',
    config = function()
      vim.g.pydocstring_formatter = 'google'
      vim.g.pydocstring_enable_mapping = 0
      local cmd_map = require('me.map_utils').cmd_map
      cmd_map({ keys = '<leader>pd', command = 'Pydocstring' })
    end,
  },

  { 'rafcamlet/nvim-luapad' },

  {
    '2kabhishek/nerdy.nvim',
    dependencies = {
      'stevearc/dressing.nvim',
      'nvim-telescope/telescope.nvim',
    },
    cmd = 'Nerdy',
    keys = {
      { '<leader>nf', '<cmd>Nerdy<CR>', desc = 'Interactively choose a glyph from the Nerd Font collection' },
    },
  },
}
