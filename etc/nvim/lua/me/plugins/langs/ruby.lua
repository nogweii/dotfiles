local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'erb-formatter', 'erb-lint', 'ruby-lsp' })
    -- TODO: ruby-lsp crashes at start, because when it re-execs itself in a
    -- bundler context, it suddenly loses the ruby-lsp bin
    -- similar: https://github.com/Shopify/ruby-lsp/issues/1713
  }
}
