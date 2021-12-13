local lsp_status_icon = require('me.lsp.utils').lsp_status_icon
local vi_mode = require('feline.providers.vi_mode')
local mat_colors = require('material.colors')

local center_pad = function(str, len, char)
    if char == nil then char = ' ' end

    local left_pad = math.ceil((len - #str) / 2)
    local right_pad = math.floor((len - #str) / 2)

    return string.rep(char, left_pad) .. str .. string.rep(char, right_pad)
end

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
    right_sep = 'slant_right_2',
  },

  lsp_icon = {
    provider = '',
    icon = function()
      return lsp_status_icon(vim.bo.filetype)
    end,
  },

  file_name = {
    provider = {
      name = 'file_info',
      opts = {
        type = 'unique',
        file_modified_icon = '💾',
        file_readonly_icon = '🔒',
      },
    },
    left_sep = 'left_rounded',
    right_sep = 'right_rounded',
    hl = {
      bg = 'black'
    }
  },

  cursor_position = {
    provider = function()
      return string.format('%3d:%-2d', unpack(vim.api.nvim_win_get_cursor(0)))
    end,
    icon = '',
    left_sep = 'slant_left_2',
    hl = {
      fg = 'black',
      bg = 'violet',
    }
  },

  diag_errors = {
    provider = 'diagnostic_errors',
    hl = { fg = 'red' }
  },
  diag_warnings = {
    provider = 'diagnostic_warnings',
    hl = { fg = 'yellow' }
  },
  diag_hints = {
    provider = 'diagnostic_hints',
    hl = { fg = 'cyan' }
  },
  diag_info = {
    provider = 'diagnostic_info',
    hl = { fg = 'skyblue' }
  },

  git_branch = {
    provider = 'git_branch',
    hl = {
      bg = mat_colors.darkorange,
      fg = mat_colors.bg,
    },
    right_sep = {
      str = 'slant_left_2',
      hl = {
        fg = mat_colors.bg,
        bg = mat_colors.darkorange,
      }
    },
    left_sep = 'slant_left_2',
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
      }
    },

    inactive = {
      { -- left
        components.file_name,
      },
      { -- middle
      },
      { -- right
      },
    }
  }
})
