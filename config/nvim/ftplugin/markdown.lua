local cmp = require('cmp')

-- Automatically save a markdown file when closing it (among others, see :help 'awa')
vim.api.nvim_create_autocmd('FileType', { pattern = 'markdown', command = 'set awa' })
-- ...or when simply switching to a different file
vim.api.nvim_create_autocmd('BufLeave', { pattern = '*.md', command = 'silent! write' })

cmp.setup.filetype('markdown', {
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
  }, {
    { name = 'emoji' },
    { name = 'buffer' },
    { name = 'path' },
    { name = 'luasnip' },
  }),
})

---Lazy function to easily create a buffer-specific mapping
---@param modes table All the vim modes the mapping is applied to
---@param mapping string The keys to bind this to
---@param desc string A friendly description to remind me of what this is
---@param mkdnFunc function the function to be invoked
local function buf_keymap(modes, mapping, desc, mkdnFunc)
  local opts = {
    desc = '󰽛 ' .. desc,
    buffer = true,
  }
  vim.keymap.set(modes, mapping, mkdnFunc, opts)
end
