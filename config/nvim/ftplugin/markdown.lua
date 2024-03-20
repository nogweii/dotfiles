local cmp = require('cmp')
local mkdnflow = require('mkdnflow')

-- Automatically save a markdown file when closing it (among others, see :help 'awa')
vim.api.nvim_create_autocmd('FileType', { pattern = 'markdown', command = 'set awa' })
-- ...or when simply switching to a different file
vim.api.nvim_create_autocmd('BufLeave', { pattern = '*.md', command = 'silent! write' })

cmp.setup.filetype('markdown', {
  sources = cmp.config.sources({
    { name = 'mkdnflow' },
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

buf_keymap({ 'i' }, '<C-h>', 'Previous cell', function()
  mkdnflow.tables.moveToCell(0, -1)
end)
buf_keymap({ 'i' }, '<C-j>', 'Next row', function()
  mkdnflow.tables.moveToCell(1, 0)
end)
buf_keymap({ 'i' }, '<C-k>', 'Previous row', function()
  mkdnflow.tables.moveToCell(-1, 0)
end)
buf_keymap({ 'i' }, '<C-l>', 'Next cell', function()
  mkdnflow.tables.moveToCell(0, 1)
end)

buf_keymap({ 'i' }, '<C-S-h>', 'New cell to left', function()
  mkdnflow.tables.addCol(-1)
end)
buf_keymap({ 'i' }, '<C-S-j>', 'New row above', function()
  mkdnflow.tables.addRow(1)
end)
buf_keymap({ 'i' }, '<C-S-k>', 'New row below', function()
  mkdnflow.tables.addRow(-1)
end)
buf_keymap({ 'i' }, '<C-S-l>', 'New cell to right', function()
  mkdnflow.tables.addCol(1)
end)

buf_keymap({ 'n' }, 'o', 'Additional list item below', function()
  mkdnflow.lists.newListItem(false, false, true, 'i', 'o')
end)
buf_keymap({ 'n' }, 'O', 'Additional list item above', function()
  mkdnflow.lists.newListItem(false, true, true, 'i', 'O')
end)

buf_keymap({ 'n', 'v' }, '<C-Space>', 'Toggle todo', function()
  mkdnflow.lists.toggleToDo(false, false, {})
end)
buf_keymap({ 'n' }, '<leader>mr', 'Rename link', function()
  mkdnflow.paths.moveSource()
end)
buf_keymap({ 'n', 'v' }, '<leader>mp', 'Create a link from clipboard', function()
  mkdnflow.links.createLink({ from_clipboard = true })
end)
buf_keymap({ 'n' }, '<leader>md', 'Destroy link', function()
  mkdnflow.links.destroyLink()
end)
buf_keymap({ 'n' }, '<leader>mn', 'Fix list numbers', function()
  mkdnflow.lists.updateNumbering()
end)
buf_keymap({ 'n' }, '<leader>my', 'Yank header as markdown link', function()
  mkdnflow.cursor.yankAsAnchorLink({})
end)
