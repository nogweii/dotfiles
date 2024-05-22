-- Bootstrap lazy.nvim by automatically cloning the git repo
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  if not vim.fn.executable('git') then
    print("Lazy.nvim not installed and git not found in PATH. Plugins aren't available!")
  else
    vim.fn.system({
      'git',
      'clone',
      '--filter=blob:none',
      'https://github.com/folke/lazy.nvim.git',
      '--branch=stable', -- latest stable release
      lazypath,
    })
    print('Lazy.nvim git repository cloned.')
  end
end
vim.opt.rtp:prepend(lazypath)

-- Now that lazy.nvim has been added to &rtp, I can use it's event system to
-- asynchronously load file plugins
require('me.autocmd_async_file_event')

-- Unicode has variants of the same glyph in multiple tables. Early pictographic symbols are an example
-- of them being duplicated in the emoji list. (think: b&w smiley face vs full-color version.) Appending
-- this codepoint will cause the previous character to always be selected from the emoji table.
local force_emoji = '\u{FE0F}'

---@type LazyConfig
local opts = {
  -- Replace some of the icons with emoji
  ui = {
    icons = {
      cmd = '⌘' .. force_emoji,
      config = '🛠' .. force_emoji,
      event = '📅',
      ft = '📂',
      init = '⚙' .. force_emoji,
      keys = '🗝' .. force_emoji,
      lazy = '💤 ',
      plugin = '🔌',
      runtime = '💻' .. force_emoji,
      source = '📄',
      start = '🚀',
      task = '📌',
    },
    custom_keys = {
      -- disable the lazygit / terminal keybindings
      ["<localleader>t"] = false,
      ["<localleader>l"] = false,

      ["<localleader>s"] = {
        --- Show the entire plugin spec in a floating window
        ---@param plugin LazyPlugin the plugin I'm highlighting
        ---@return LazyFloat
        function(plugin)
          local float = require("lazy.view.float")({})
          local lines = require("lazy.view.text").new()
          lines.padding = 2

          lines:nl()
          lines:append("Plugin spec for ", "LazyH2")
          lines:append(plugin[1], "Title"):nl():nl()
          lines:append(vim.inspect(plugin))

          lines:render(float.buf)

          vim.bo[float.buf].modifiable = false
          return float
        end,
        desc = "Print the plugin spec"
      },

      ["<localleader>o"] = {
        --- Show resulting plugin options in a floating window
        ---@param plugin LazyPlugin the plugin I'm highlighting
        ---@return LazyFloat
        function(plugin)
          local float = require("lazy.view.float")({})
          local lines = require("lazy.view.text").new()
          lines.padding = 2

          lines:nl()
          lines:append("Final options for ", "LazyH2")
          lines:append(plugin[1], "Title"):nl():nl()
          local Plugin = require("lazy.core.plugin")
          lines:append(vim.inspect(Plugin.values(plugin, "opts", false)))

          lines:render(float.buf)

          vim.bo[float.buf].modifiable = false
          return float
        end,
        desc = "Print the resulting opts for the plugin"
      }
    },
  },

  defaults = {
    version = '*',
  },
  change_detection = {
    -- don't need a notification whenever a plugin file changes
    notify = false,
  },
  performance = {
    rtp = {
      paths = { '/usr/lib/nvim' },
      disabled_plugins = {
        'gzip',
        'matchit',
        'matchparen',
        'netrwPlugin',
        'tarPlugin',
        'tohtml',
        'tutor',
        'zipPlugin',
      },
    },
  },
}

require('lazy').setup('me.plugins', opts)
