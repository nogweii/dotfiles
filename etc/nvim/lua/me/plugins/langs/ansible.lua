local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec
return {
  { 'mfussenegger/nvim-ansible' },

  {
    'WhoIsSethDaniel/mason-tool-installer.nvim',
    opts = add_ensure({ 'ansiblels', 'ansible-lint', 'jinja_lsp' }),
  },
}
