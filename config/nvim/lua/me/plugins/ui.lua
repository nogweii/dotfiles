return {
  -- a very customizable status bar framework for Neovim written in Lua
  {
    'feline-nvim/feline.nvim',
    config = function()
      require('me.settings.feline')
    end,
    dependencies = { 'nvim-tree/nvim-web-devicons', 'lewis6991/gitsigns.nvim' },
  },

  {
    'kevinhwang91/nvim-bqf',
    config = function()
      require('bqf').setup()
    end,
  },

  {
    'stevearc/dressing.nvim',
    lazy = true,
    init = function()
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.select = function(...)
        require('lazy').load({ plugins = { 'dressing.nvim' } })
        return vim.ui.select(...)
      end
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.input = function(...)
        require('lazy').load({ plugins = { 'dressing.nvim' } })
        return vim.ui.input(...)
      end
    end,
  },

  -- pretty replacement for the vim notifications messages
  {
    'rcarriga/nvim-notify',
    opts = {
      timeout = 3000,
      render = 'wrapped-compact',
      stages = 'slide',

      max_height = function()
        return math.floor(vim.o.lines * 0.5)
      end,
      max_width = function()
        return math.floor(vim.o.columns * 0.5)
      end,
      on_open = function(win)
        vim.api.nvim_win_set_config(win, { zindex = 100 })
      end,
    },
    lazy = false,
    config = function()
      vim.notify = require('notify')
    end,
  },

  -- preview colors inline in the editor
  {
    'NvChad/nvim-colorizer.lua',
    opts = {},
  },

  -- a very beautiful tabline
  { 'romgrk/barbar.nvim', dependencies = { 'nvim-tree/nvim-web-devicons' } },
}
