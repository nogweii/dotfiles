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

configs.zk.index = function()
  vim.lsp.buf.execute_command({
    command = "zk.index",
    arguments = {vim.api.nvim_buf_get_name(0)},
  })
end

configs.zk.new = function(...)
  vim.lsp.buf_request(0, 'workspace/executeCommand',
    {
        command = "zk.new",
        arguments = {
            vim.api.nvim_buf_get_name(0),
            ...
        },
    },
    function(_, _, result)
      if not (result and result.path) then return end
      vim.cmd("edit " .. result.path)
    end
  )
end

lspconfig.zk.setup(my_lsp.make_config("zk"))
