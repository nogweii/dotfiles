local cmp = require('cmp')

-- Automatically save a markdown file when closing it (among others, see :help 'awa')
vim.api.nvim_create_autocmd('FileType', { pattern = 'markdown', command = 'set awa' })
-- ...or when simply switching to a different file
vim.api.nvim_create_autocmd('BufLeave', { pattern = '*.md', command = 'silent! write' })

cmp.setup.filetype('markdown', {
  sources = cmp.config.sources({
    { name = 'mkdnflow' },
    { name = 'buffer' },
    { name = 'path' },
  }),
})

--[[
        MkdnTableNextCell = { 'i', '<Tab>' },
        MkdnTablePrevCell = { 'i', '<S-Tab>' },
        MkdnTableNextRow = false,
        MkdnTablePrevRow = { 'i', '<M-CR>' },
        MkdnTableNewRowBelow = { 'n', '<leader>ir' },
        MkdnTableNewRowAbove = { 'n', '<leader>iR' },
        MkdnTableNewColAfter = { 'n', '<leader>ic' },
        MkdnTableNewColBefore = { 'n', '<leader>iC' },

        MkdnNewListItemBelowInsert = { 'n', 'o' },
        MkdnNewListItemAboveInsert = { 'n', 'O' },

  todo: this, but powered by headline's treesitter query
        MkdnNextHeading = { 'n', ']' },
        MkdnPrevHeading = { 'n', '[' },

        MkdnEnter = { { 'n', 'v' }, '<CR>' },
        MkdnCreateLinkFromClipboard = { { 'n', 'v' }, '<leader>p' },

        MkdnToggleToDo = { { 'n', 'v' }, '<C-Space>' },

        MkdnMoveSource = { 'n', '<F2>' },
--]]
