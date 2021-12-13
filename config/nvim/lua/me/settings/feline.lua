local lsp_servers = require('nvim-lsp-installer.servers')
local all_available_lsp_servers = lsp_servers.get_available_servers()
local all_installed_lsp_servers = lsp_servers.get_installed_server_names()

-- LSP icon logic:
-- * hidden if there are no relevant LSP servers at all
-- * grayed out if one is available to be installed but is not
-- * red if one is installed but not active
-- * blue if an LSP server is actively attached to the buffer
local function lsp_status_icon(filetype)
  icon_data = {
    hl = {},
    always_visible = true, -- there is no text in this component, just an icon
  }

  if next(vim.lsp.buf_get_clients(0)) ~= nil then
    -- an LSP server is installed & actively connected, hurrah!
    icon_data.str = '力'
    icon_data.hl.fg = 'skyblue'
    icon_data.hl.name = "LspStatusIconRunning"
  else
    for n, server in pairs(all_available_lsp_servers) do
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

local components = {
  vi_mode = {
    provider = 'vi_mode',
    hl = function()
      return {
        name = require('feline.providers.vi_mode').get_mode_highlight_name(),
        fg = require('feline.providers.vi_mode').get_mode_color(),
        style = 'bold'
      }
    end,
    right_sep = ' ',
  },

  lsp_icon = {
    provider = '',
    icon = function()
      return lsp_status_icon(vim.bo.filetype)
    end
  },

  lsp_name = {
    provider = 'lsp_client_names',
    left_sep = ' ',
    icon = ' ',
    hl = {
      fg = 'yellow'
    }
  },

  file_name = {
    provider = {
      name = 'file_info',
      opts = {
        type = 'unique',
        file_modified_icon = '💾',
        file_readonly_icon = '🔒',
      }
    },
  },

  cursor_position = {
    provider = function()
      return string.format('%3d:%-2d', unpack(vim.api.nvim_win_get_cursor(0)))
    end,
    icon = ''
  }
}

require('feline').setup({
  components = {
    active = {
      { -- left
        components.vi_mode,
        components.lsp_client_names,
      },
      { -- middle
        components.file_name,
      },
      { -- right
        components.cursor_position,
        components.lsp_icon,
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
