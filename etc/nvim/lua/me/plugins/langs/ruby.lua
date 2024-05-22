local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'solargraph', 'erb-formatter', 'erb-lint', 'ruby-lsp', 'rufo' })
  }
}
