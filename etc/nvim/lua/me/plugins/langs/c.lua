-- Improving the C & C++ editing experience

local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'cpplint', 'clangd' })
    -- TODO: ccls ? is it usefully different from clangd?
  }
}
