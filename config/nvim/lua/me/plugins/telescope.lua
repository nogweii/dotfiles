---@type LazySpec
return {
  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim' },
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
              bottom_pane = {
                height = 15,
                preview_cutoff = 120,
                prompt_position = 'bottom',
              },
            },
            prompt_title = false,
          },
        },
      })

      require('telescope').load_extension('notify')
    end,
  },
}
