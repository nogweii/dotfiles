require('formatter').setup({
  log_level = vim.log.levels.WARN,

  -- All formatter configurations are opt-in, they must be specified here.
  -- If a filetype table has multiple entries, they will be ran in order.
  filetype = {
    -- The special "*" filetype defines formatter configurations for all filetypes
    ['*'] = {
      require('formatter.filetypes.any').remove_trailing_whitespace,
    },

    lua = {
      require('formatter.filetypes.lua').stylua,
    },

    terraform = {
      function()
        return {
          exe = 'terraform',
          args = { 'fmt', '-' },
          stdin = true,
        }
      end,
    },

    python = {
      require('formatter.filetypes.python').black,
    },

    ruby = {
      -- running standardrb is *almost* the same as rubocop
      function()
        local rubocop_config = require('formatter.filetypes.ruby').rubocop()
        rubocop_config.exe = 'standardrb'
        rubocop_config.args[1] = '--fix'
        return rubocop_config
      end,
    },
  },
})
