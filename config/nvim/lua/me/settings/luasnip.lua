local luasnip = require('luasnip')

luasnip.config.set_config({
  updateevents = 'TextChanged,TextChangedI',
})

-- One peculiarity of honza/vim-snippets is that the file containing global
-- snippets is _.snippets, so we need to tell luasnip that the filetype "_"
-- contains global snippets:
luasnip.filetype_extend('all', { '_' })

require('luasnip.loaders.from_vscode').lazy_load()
require('luasnip.loaders.from_snipmate').lazy_load()
require('luasnip.loaders.from_lua').lazy_load()
