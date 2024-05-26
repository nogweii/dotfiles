---@type LazySpec
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
    'yorickpeterse/nvim-pqf',
    opts = {},
    event = 'AsyncFileLoad',
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
    opts = {
      user_default_options = {
        css = true,
        names = false,
      },
    },
    event = 'AsyncFileLoad',
  },

  -- flexible yet pretty list of buffers in the top of the screen
  -- (filling in tabline, even though it's only ever 1 tab (usually))
  {
    'akinsho/bufferline.nvim',
    version = '*',
    dependencies = 'nvim-tree/nvim-web-devicons',
    lazy = false,
    keys = {
      { '[b',    '<cmd>BufferLineCyclePrev<cr>', desc = 'Prev buffer' },
      { ']b',    '<cmd>BufferLineCycleNext<cr>', desc = 'Next buffer' },
      { '<C-n>', '<cmd>BufferLineCycleNext<cr>', desc = 'Next buffer' },
      { '<C-p>', '<cmd>BufferLineCyclePrev<cr>', desc = 'Prev buffer' },
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
        show_numbers = true,    -- Enable 'number' for the window while peeking
        show_cursorline = true, -- Enable 'cursorline' for the window while peeking
      })
    end,
  },

  -- in visual mode, show all of the whitespace characters
  {
    'mcauley-penney/visual-whitespace.nvim',
    opts = {},
    -- lazily load the plugin only I enter Visual mode the first time
    lazy = true,
    init = function()
      vim.api.nvim_create_autocmd('ModeChanged', {
        pattern = '*:[vV]',
        once = true,
        callback = function()
          require('visual-whitespace').highlight_ws()
        end,
      })
    end,
  },

  -- using the manual fold API, make smart folds calculated with indentation and TreeSitter
  {
    'kevinhwang91/nvim-ufo',
    dependencies = 'kevinhwang91/promise-async',
    event = 'AsyncFileLoad',
    keys = {
      {
        'zR',
        function()
          require('ufo').openAllFolds()
        end,
        desc = 'Open all folds',
      },
      {
        'zM',
        function()
          require('ufo').closeAllFolds()
        end,
        desc = 'Close all folds',
      },
      {
        'zZ',
        function()
          require('ufo').peekFoldedLinesUnderCursor()
        end,
        desc = 'Peek folded lines under cursor',
      },
      {
        'zr',
        function()
          require('ufo').openFoldsExceptKinds()
        end,
        desc = 'Fold less',
      },
      {
        'zm',
        function()
          require('ufo').closeFoldsWith()
        end,
        desc = 'Fold more',
      },
    },
    opts = {
      provider_selector = function(bufnr, filetype, buftype)
        local function handleFallbackException(targetBufnr, err, providerName)
          if type(err) == 'string' and err:match('UfoFallbackException') then
            return require('ufo').getFolds(targetBufnr, providerName)
          else
            return require('promise').reject(err)
          end
        end

        -- Try to get ufo to generate folds via the LSP. But if that
        -- fails, try again using Treesitter. Failing *that*, just
        -- use indentation as the fold method.
        ---@param targetBufnr number
        ---@return Promise
        local function findFirstAvailableProvider(targetBufnr)
          return require('ufo')
              .getFolds(targetBufnr, 'lsp')
              :catch(function(err)
                return handleFallbackException(targetBufnr, err, 'treesitter')
              end)
              :catch(function(err)
                return handleFallbackException(targetBufnr, err, 'indent')
              end)
        end

        return (filetype == '' or buftype == 'nofile') and 'indent' -- only use indent until a file is opened
            or vim.b[bufnr].ufo_provider                            -- Allow a buffer to have specific overrides
            or findFirstAvailableProvider                           -- And lastly, try to automatically determine the available provider
      end,
    },
  },

  {
    'luukvbaal/statuscol.nvim',
    opts = function()
      local builtin = require('statuscol.builtin')
      return {
        setopt = true,
        -- override the default list of segments with:
        -- number-less fold indicator, then signs, then line number & separator
        segments = {
          { text = { builtin.foldfunc }, click = 'v:lua.ScFa' },
          { text = { '%s' },             click = 'v:lua.ScSa' },
          {
            text = { builtin.lnumfunc, ' ' },
            condition = { true, builtin.not_empty },
            click = 'v:lua.ScLa',
          },
        },
      }
    end,
  },

  {
    'nvim-zh/colorful-winsep.nvim',
    config = true,
    event = { 'WinNew', 'WinClosed' },
  },

  {
    'mvllow/modes.nvim',
    opts = {},
  },
}
