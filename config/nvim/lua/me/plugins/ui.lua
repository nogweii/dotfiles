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

  {
    'akinsho/bufferline.nvim',
    version = '*',
    dependencies = 'nvim-tree/nvim-web-devicons',
    lazy = false,
    keys = {
      { '[b', '<cmd>BufferLineCyclePrev<cr>', desc = 'Prev buffer' },
      { ']b', '<cmd>BufferLineCycleNext<cr>', desc = 'Next buffer' },
    },
    opts = {
      options = {
        close_command = function(n)
          require('mini.bufremove').delete(n, false)
        end,
        right_mouse_command = function(n)
          require('mini.bufremove').delete(n, false)
        end,
        diagnostics = 'nvim_lsp',
        always_show_bufferline = false,
        offsets = {
          {
            filetype = 'neo-tree',
            text = 'Neo-tree',
            highlight = 'Directory',
            text_align = 'left',
          },
        },
      },
    },
    config = function(_, opts)
      require('bufferline').setup(opts)
      -- Fix bufferline when restoring a session
      vim.api.nvim_create_autocmd({ 'BufAdd', 'UIEnter', 'VimEnter' }, {
        callback = function()
          vim.schedule(function()
            require('bufferline').setup(opts)
          end)
        end,
      })
    end,
  },

  { 'jiaoshijie/undotree', cmd = 'UndotreeToggle' },

  -- when typing `:<number>` scroll to that line, only while in command mode
  -- which allows easy peeking to another location in the file
  {
    'nacro90/numb.nvim',
    config = function()
      require('numb').setup({
        show_numbers = true, -- Enable 'number' for the window while peeking
        show_cursorline = true, -- Enable 'cursorline' for the window while peeking
      })
    end,
  },
}
