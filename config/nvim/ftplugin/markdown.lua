local cmp = require('cmp')
local mkdnflow = require('mkdnflow')
local tablemd = require('tablemd')

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

-- These are do-nothing maps, yes. But they also define a description!
-- That's the important part.
vim.keymap.set('n', '<leader>m', function() end, { desc = '󰽛 Markdown', buffer = true })
vim.keymap.set('n', '<leader>mt', function() end, { desc = 'Tables', buffer = true })

---Lazy function to easily create a buffer-specific mapping
---@param modes table All the vim modes the mapping is applied to
---@param mapping string The keys to bind this to
---@param desc string A friendly description to remind me of what this is
---@param mkdnFunc function the function to be invoked
local function buf_keymap(modes, mapping, desc, mkdnFunc)
  local opts = {
    desc = desc,
    buffer = true,
  }
  vim.keymap.set(modes, '<leader>m' .. mapping, mkdnFunc, opts)
end

buf_keymap({ 'n' }, 'to', 'Add table row below', function()
  tablemd.insertRow(false)
end)
buf_keymap({ 'n' }, 'tO', 'Add table row above', function()
  tablemd.insertRow(true)
end)
buf_keymap({ 'n' }, 'ti', 'Add table column to the right', function()
  tablemd.insertColumn(true)
end)
buf_keymap({ 'n' }, 'tI', 'Add table column to the left', function()
  tablemd.insertColumn(false)
end)
buf_keymap({ 'n' }, 'tf', 'Reformat the table', function()
  tablemd.format()
end)
buf_keymap({ 'n' }, 'td', 'Delete current table column', function()
  tablemd.deleteColumn()
end)
buf_keymap({ 'n' }, 'te', 'Edit current cell', function()
  require('edit-markdown-table').edit_cell()
end)

buf_keymap({ 'n' }, 'c', 'Toggle checklist', function()
  -- this combo will make this toggling dot-repeatable
  vim.go.operatorfunc = "v:lua.require('markdown-togglecheck').toggle"
  return 'g@l'
end)

vim.keymap.set({ 'n' }, 'o', function()
  mkdnflow.lists.newListItem(false, false, true, 'i', 'o')
end, { desc = 'Additional list item below', buffer = true })
vim.keymap.set({ 'n' }, 'O', function()
  mkdnflow.lists.newListItem(false, true, true, 'i', 'O')
end, { desc = 'Additional list item above', buffer = true })

buf_keymap({ 'n', 'v' }, 'p', 'Paste link from clipboard', function()
  mkdnflow.links.createLink({ from_clipboard = true })
end)
buf_keymap({ 'n' }, 'P', 'Paste clipboard image', function()
  require('img-clip').pasteImage()
end)

buf_keymap({ 'n' }, 'r', 'Rename link', function()
  mkdnflow.paths.moveSource()
end)
buf_keymap({ 'n' }, 'd', 'Destroy link', function()
  mkdnflow.links.destroyLink()
end)
buf_keymap({ 'n' }, 'n', 'Fix list numbers', function()
  mkdnflow.lists.updateNumbering()
end)
buf_keymap({ 'n' }, 'y', 'Yank header as link', function()
  mkdnflow.cursor.yankAsAnchorLink({})
end)
vim.keymap.set({ 'n', 'v' }, '<CR>', function()
  local range
  if vim.v.count == 0 then
    range = false
  else
    range = vim.v.count
  end
  mkdnflow.links.followLink({ range = range })
end, { desc = 'Follow or create a link', buffer = true })
buf_keymap({ 'n' }, 'v', 'Preview with :Glow', function()
  require('glow').execute({ bang = true, fargs = {} })
end)
