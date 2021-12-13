local lsp_servers = require('nvim-lsp-installer.servers')
local all_available_lsp_servers = lsp_servers.get_available_servers()
local all_installed_lsp_servers = lsp_servers.get_installed_server_names()

M = {}

-- LSP icon logic:
-- * hidden if there are no relevant LSP servers at all
-- * grayed out if one is available to be installed but is not
-- * red if one is installed but not active
-- * blue if an LSP server is actively attached to the buffer
function M.lsp_status_icon(filetype)
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

return M
