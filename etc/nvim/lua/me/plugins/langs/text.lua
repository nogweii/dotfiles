local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'editorconfig-checker', 'harper-ls', 'typos-lsp' })
  },
}
