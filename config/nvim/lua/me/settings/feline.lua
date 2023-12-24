local vi_mode = require('feline.providers.vi_mode')

local center_pad = function(str, len, char)
  if char == nil then
    char = ' '
  end

  local left_pad = math.ceil((len - #str) / 2)
  local right_pad = math.floor((len - #str) / 2)

  return string.rep(char, left_pad) .. str .. string.rep(char, right_pad)
end

local left_half_bar = '▌'

local components = {
  vi_mode = {
    provider = function()
      return center_pad(vi_mode.get_vim_mode(), 9)
    end,
    hl = function()
      return {
        name = vi_mode.get_mode_highlight_name(),
        bg = vi_mode.get_mode_color(),
        -- style = 'bold'
      }
    end,
    -- TODO: somehow determine the darker version of the vim mode color
    -- left_sep = left_half_bar,
    right_sep = '',
  },

  lsp_icon = {
    provider = '',
    icon = function()
      if next(vim.lsp.buf_get_clients(0)) ~= nil then
        -- an LSP server is installed & actively connected, hurrah!
        return {
          str = '󰒋',
          hl = {
            bg = '#343a40',
            fg = 'skyblue',
            name = 'LspStatusIconRunning',
          },
          always_visible = true, -- there is no text in this component, just an icon
        }
      else
        return {}
      end
    end,
    left_sep = function()
      if next(vim.lsp.buf_get_clients(0)) ~= nil then
        -- an LSP server is installed & actively connected, hurrah!
        return {
          str = left_half_bar,
          hl = {
            fg = '#adb5bd',
            bg = '#343a40',
          },
          always_visible = true,
        }
      else
        return {}
      end
    end,
  },

  file_name = {
    provider = {
      name = 'file_info',
      opts = {
        type = 'relative',
        file_modified_icon = '💾',
        file_readonly_icon = '🔒',
      },
    },
    left_sep = {
      str = '▐',
      fg = '#343a40',
    },
    right_sep = {
      str = left_half_bar,
      fg = '#343a40',
    },
    hl = {
      bg = '#343a40',
    },
  },

  cursor_position = {
    provider = function()
      return string.format('%3d:%-2d', unpack(vim.api.nvim_win_get_cursor(0)))
    end,
    icon = '',
    left_sep = {
      str = left_half_bar,
      hl = {
        fg = '#6f42c1',
        bg = 'violet',
      },
    },
    hl = {
      fg = 'black',
      bg = 'violet',
    },
  },

  diag_errors = {
    provider = 'diagnostic_errors',
    hl = { fg = 'red' },
  },
  diag_warnings = {
    provider = 'diagnostic_warnings',
    hl = { fg = 'yellow' },
  },
  diag_hints = {
    provider = 'diagnostic_hints',
    hl = { fg = 'cyan' },
  },
  diag_info = {
    provider = 'diagnostic_info',
    hl = { fg = 'skyblue' },
  },

  git_branch = {
    provider = 'git_branch',
    hl = {
      bg = '#fd7e14',
      fg = '#212529',
    },
    left_sep = {
      str = left_half_bar,
      hl = {
        fg = '#984c0c',
        bg = '#fd7e14',
      },
    },
  },
}

require('feline').setup({
  components = {
    active = {
      { -- left
        components.vi_mode,
      },
      { -- middle
        components.file_name,
      },
      { -- right
        components.git_branch,

        components.lsp_icon,
        components.diag_errors,
        components.diag_warnings,
        components.diag_hints,
        components.diag_info,

        components.cursor_position,
      },
    },

    inactive = {
      { -- left
        components.file_name,
      },
      { -- middle
      },
      { -- right
      },
    },
  },
})
