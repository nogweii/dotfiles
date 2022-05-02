local setup_options = {}

if vim.fn.executable("vscode-css-languageserver") == 1 then
  -- the Arch Linux package installs the language server with a slightly
  -- different name in $PATH
  setup_options['cmd'] = {"vscode-css-languageserver", "--stdio"}
end

return setup_options
