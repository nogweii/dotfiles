local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {

  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'shfmt', 'shellharden', 'bashls', 'termux-language-server' }),
  },

}
