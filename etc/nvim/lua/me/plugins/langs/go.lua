local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ "delve", "gopls", "gomodifytags", "gofumpt", "iferr", "impl", "goimports" })
  }
}
