---@type LazySpec
return {
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim',
      'jvgrootveld/telescope-zoxide'
    },
    config = function()
      local actions = require('telescope.actions')

      require('telescope').setup({
        defaults = {
          mappings = {
            i = {
              ['<esc>'] = actions.close,
            },
          },
        },
        pickers = {
          live_grep = {
            theme = 'ivy',
            scroll_strategy = 'limit',
            layout_config = {
              height = 15,
              prompt_position = 'bottom',
              preview_width = 0,
            },
            prompt_title = false,
          },
        },
      })

      require('telescope').load_extension('notify')
      require("telescope").load_extension('zoxide')
    end,
  },
}
