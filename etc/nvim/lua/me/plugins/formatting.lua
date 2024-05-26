---@type LazySpec
return {
  {
    'johmsalas/text-case.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
    config = function()
      require('textcase').setup({})
      require('telescope').load_extension('textcase')
    end,
    keys = {
      '<leader>tc',
      { '<leader>tc.', '<cmd>TextCaseOpenTelescope<CR>', mode = { 'n', 'x' }, desc = 'Telescope' },
    },
  },

  -- Automatic semi-smart indentation settings for a buffer
  {
    'NMAC427/guess-indent.nvim',
    opts = {
      auto_cmd = true,
      -- editorconfig is the source of truth
      override_editorconfig = false,
    },
    event = 'AsyncFileLoad',
  },

  {
    'danymat/neogen',
    opts = {
      snippet_engine = 'luasnip',
    },
    keys = {
      { '<leader>tgg', '<cmd>Neogen<CR>', desc = 'Generate doc annotation' },
      { 'ZN',          '<cmd>Neogen<CR>', desc = 'Generate doc annotation' },
    },
    cmd = 'Neogen',
  },

  {
    'LudoPinelli/comment-box.nvim',
    opts = {
      comment_style = 'auto',
    },
    keys = {
      -- manage comment boxes
      { 'gcbc', '<cmd>CBcatalog<CR>', desc = 'Show catalog' },
      { 'gcbd', '<cmd>CBd<CR>',       desc = 'Delete box' },
      { 'gcby', '<cmd>CBy<CR>',       desc = 'Yank contents of box' },

      -- and bind various styles
      { 'gcbb', '<cmd>CBlcbox10<CR>', desc = 'Boxed' },
      { 'gcbt', '<cmd>CBllline<CR>',  desc = 'Titled' },
      { 'gcbl', '<cmd>CBline<CR>',    desc = 'Simple Line' },
      { 'gcbm', '<cmd>CBllbox14<CR>', desc = 'Marked' },
    },
  },
}
