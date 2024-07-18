local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {

  -- Additional LSP setup for LuaLS.
  {
    "folke/lazydev.nvim",
    dependencies = {
      -- these repos contain extra typings
      { "Bilal2453/luvit-meta",        lazy = true },
      { "justinsgithub/wezterm-types", lazy = true },
    },
    ft = "lua", -- only load on lua files
    opts = {
      library = {
        -- Load luvit types when the `vim.uv` word is found
        { path = "luvit-meta/library", words = { "vim%.uv" } },

        -- Load the wezterm types when the `wezterm` module is required
        { path = "wezterm-types",      mods = { "wezterm" } },
      },
    },
  },

  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'stylua', 'lua_ls' }),
  },
}
