local wk = require 'which-key'
local map = require('me.settings.map').cmd_map

wk.register({
  f = 'Fuzzy find a note',
}, { prefix = ';z' })

_G.my_zk = {}

_G.my_zk.find = function()
  local pickers = require 'telescope.pickers'
  local finders = require 'telescope.finders'
  local conf = require('telescope.config').values

  pickers.new({}, {
    prompt_title = 'Find notes',
    finder = finders.new_oneshot_job({
      'zk',
      'list',
      '-q',
      '-P',
      '--format',
      '{{ abs-path }}\t{{ title }}',
    }, {}),
    sorter = conf.generic_sorter {},
    previewer = conf.file_previewer {},
  }):find()
end

map{keys = "<leader>zf", to = "lua my_zk.find()", plugins = true}

-- Don't need to override any special settings for the LSP setup, so return an
-- empty table
return {}
