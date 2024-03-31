---@type LazySpec[]
return {
  {
    {
      'echasnovski/mini.nvim',
      dependencies = {
        -- Not directly used, but this has a number of well-maintained queries to power mini.ai
        'nvim-treesitter/nvim-treesitter-textobjects',
      },
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

        local ai = require('mini.ai')
        ai.setup({
          n_lines = 500,
          custom_textobjects = {
            o = ai.gen_spec.treesitter({
              a = { '@block.outer', '@conditional.outer', '@loop.outer' },
              i = { '@block.inner', '@conditional.inner', '@loop.inner' },
            }, {}),
            f = ai.gen_spec.treesitter({ a = '@function.outer', i = '@function.inner' }, {}),
            c = ai.gen_spec.treesitter({ a = '@class.outer', i = '@class.inner' }, {}),
            u = ai.gen_spec.function_call(), -- u for "Usage"
          },
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
            { mode = 'n', keys = '<Leader>g', desc = '󰊢 Git' },
            { mode = 'n', keys = '<Leader>l', desc = ' LSP' },
            { mode = 'n', keys = '<Leader>m', desc = '󰽛 Markdown' },
            { mode = 'n', keys = '<Leader>mt', desc = 'Tables' },
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

        require('mini.indentscope').setup({
          symbol = '│',
          options = { try_as_border = true },
          draw = { delay = 250 },
        })
        vim.api.nvim_create_autocmd('FileType', {
          pattern = {
            'help',
            'alpha',
            'dashboard',
            'neo-tree',
            'Trouble',
            'trouble',
            'lazy',
            'mason',
            'notify',
            'toggleterm',
            'lazyterm',
          },
          callback = function()
            ---@diagnostic disable-next-line: inject-field
            vim.b.miniindentscope_disable = true
          end,
        })

        require('mini.pairs').setup({
          modes = { insert = true, command = true, terminal = false },
        })

        require('mini.comment').setup({
          custom_commentstring = function()
            return require('ts_context_commentstring.internal').calculate_commentstring() or vim.bo.commentstring
          end,
        })

        require('mini.cursorword').setup({
          -- Delay (in ms) between cursor movement and word highlighting
          delay = 250,
        })

        local animate = require('mini.animate')
        animate.setup({
          resize = {
            timing = animate.gen_timing.linear({ duration = 100, unit = 'total' }),
          },
          scroll = {
            enable = false,
          },
        })
      end,
      keys = {
        {
          'ZD',
          function()
            local bufrm = require('mini.bufremove').delete
            if vim.bo.modified then
              local choice = vim.fn.confirm(('Save changes to %q?'):format(vim.fn.bufname()), '&Yes\n&No\n&Cancel')
              if choice == 1 then -- Yes
                vim.cmd.write()
                bufrm(0)
              elseif choice == 2 then -- No
                bufrm(0, true)
              end
            else
              bufrm(0)
            end
          end,
          desc = 'Delete Buffer',
        },
      },
    },
  },
}
