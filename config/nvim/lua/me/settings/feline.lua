local vi_mode = require('feline.providers.vi_mode')
local mat_colors = require('material.colors')
local lsp_servers = require('nvim-lsp-installer.servers')
local all_available_lsp_servers = lsp_servers.get_available_servers()
local all_installed_lsp_servers = lsp_servers.get_installed_server_names()


-- LSP icon logic:
-- * hidden if there are no relevant LSP servers at all
-- * grayed out if one is available to be installed but is not
-- * red if one is installed but not active
-- * blue if an LSP server is actively attached to the buffer
local function lsp_status_icon(filetype)
  local icon_data = {
    hl = {},
    always_visible = true, -- there is no text in this component, just an icon
  }

  if next(vim.lsp.buf_get_clients(0)) ~= nil then
    -- an LSP server is installed & actively connected, hurrah!
    icon_data.str = '力'
    icon_data.hl.fg = 'skyblue'
    icon_data.hl.name = "LspStatusIconRunning"
  else
    for _, server in pairs(all_available_lsp_servers) do
      if vim.tbl_contains(server.languages, filetype) then
        if vim.tbl_contains(all_installed_lsp_servers, server.name) then
          -- the LSP server is installed, but it's not an active client.
          -- something probably went wrong to get here
          icon_data.str = '力'
          icon_data.hl.fg = 'red'
          icon_data.hl.name = "LspStatusIconError"

          -- stop checking for other LSP servers, an error is important to check out
          break
        else
          -- an LSP server is available, but it's not installed.
          -- let me know I could install one
          icon_data.str = '年'
          icon_data.hl.fg = "#4e4e4e"
          icon_data.hl.name = "LspStatusIconAvailale"
        end
      end
    end
  end

  return icon_data
end

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
