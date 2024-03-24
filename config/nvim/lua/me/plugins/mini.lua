---@type LazySpec[]
return {
  {
    {
      'echasnovski/mini.nvim',
      version = '*',
      lazy = false,
      config = function()
        require('mini.align').setup({
          mappings = {
            start = '', -- ga is already mapped to show the Unicode character, so don't overwrite that
            start_with_preview = 'gA',
          },
        })

        require('mini.bufremove').setup()
        require('mini.bracketed').setup({
          -- Disable a few targets that I don't care about
          -- stylua: ignore start
          buffer  = { suffix = '' }, -- this is handled by bufferline
          file    = { suffix = '' },
          oldfile = { suffix = '' },
          undo    = { suffix = '' },
          window  = { suffix = '' },
          yank    = { suffix = '' },
          -- stylua: ignore end
        })

        -- Key bindings help & reminder
        local clue = require('mini.clue')
        clue.setup({
          triggers = {
            { mode = 'n', keys = '<Leader>' },
            { mode = 'x', keys = '<Leader>' },
            { mode = 'i', keys = '<C-x>' },
            { mode = 'n', keys = 'g' },
            { mode = 'x', keys = 'g' },
            { mode = 'n', keys = "'" },
            { mode = 'n', keys = '`' },
            { mode = 'x', keys = "'" },
            { mode = 'x', keys = '`' },
            { mode = 'n', keys = '"' },
            { mode = 'x', keys = '"' },
            { mode = 'i', keys = '<C-r>' },
            { mode = 'c', keys = '<C-r>' },
            { mode = 'n', keys = '<C-w>' },
            { mode = 'n', keys = 'z' },
            { mode = 'x', keys = 'z' },
            { mode = 'n', keys = 'Z' },
            { mode = 'n', keys = '[' },
            { mode = 'n', keys = ']' },
          },

          clues = {
            -- Enhance this by adding descriptions for <Leader> mapping groups
            clue.gen_clues.builtin_completion(),
            clue.gen_clues.g(),
            clue.gen_clues.marks(),
            clue.gen_clues.registers(),
            clue.gen_clues.windows(),
            clue.gen_clues.z(),
            { mode = 'n', keys = '<Leader>g', desc = '+Git' },
            { mode = 'n', keys = '<Leader>l', desc = '+LSP' },
          },

          window = {
            -- Show window immediately
            delay = 500,
            config = {
              -- Compute window width automatically
              width = 'auto',
              -- Use double-line border
              border = 'double',
            },
          },
        })
      end,
      keys = {
        {
          'ZD',
          function()
            require('mini.bufremove').delete(0, false)
          end,
          desc = 'Delete Buffer',
        },
      },
    },
  },
}
