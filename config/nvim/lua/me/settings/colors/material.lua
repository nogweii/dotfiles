vim.g.material_style = "darker"
require('material').setup({
  italics = {
    comments = true,
  }
})
vim.cmd 'colorscheme material'
