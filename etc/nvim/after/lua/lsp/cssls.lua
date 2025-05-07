local setup_options = {
  settings = {
    css = {
      format = {
        enable = false,
      }
    },
    less = {
      format = {
        enable = false,
      }
    },
    scss = {
      format = {
        enable = false,
      }
    }
  }
}

if vim.fn.executable('vscode-css-languageserver') == 1 then
  -- the Arch Linux package installs the language server with a slightly
  -- different name in $PATH
  setup_options['cmd'] = { 'vscode-css-languageserver', '--stdio' }
end

return setup_options
