local setup_options = {}

if vim.fn.executable("vscode-json-languageserver") == 1 then
  -- the Arch Linux package installs the language server with a slightly
  -- different name in $PATH
  setup_options["cmd"] = { "vscode-json-languageserver", "--stdio" }
end

setup_options["settings"] = {
  json = {
    schemas = require("schemastore").json.schemas(),
    validate = { enable = true },
  },
}

return setup_options
