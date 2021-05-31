local lspconfig = require('lspconfig')
local configs = require('lspconfig/configs')
local my_lsp = require('me.settings.lsp')

require("zk").setup({
    debug = false,
    log = true,
    default_keymaps = false,
    default_notebook_path = vim.env.ZK_NOTEBOOK_DIR,
    fuzzy_finder = "telescope",
    link_format = "markdown"
})

configs.zk = {
  default_config = {
    cmd = {'zk', 'lsp'},
    filetypes = {'markdown'},
    root_dir = lspconfig.util.root_pattern(".zk"),
    settings = {}
  }
}

lspconfig.zk.setup(my_lsp.make_config())

--[[ FileType
    python    lua require'lspconfig'["pylsp"].manager.try_add() ]]

