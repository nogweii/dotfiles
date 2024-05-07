---@type LazySpec
return {
  {
    'zbirenbaum/copilot-cmp',
    opts = {},
    event = 'AsyncFileLoad',
    dependencies = {
      {
        'zbirenbaum/copilot.lua',
        cmd = 'Copilot',
        event = 'InsertEnter',
        opts = {
          filetypes = {
            yaml = false,
            markdown = false,
            help = false,
            gitcommit = false,
            gitrebase = false,
            hgcommit = false,
            svn = false,
            cvs = false,
            ['.'] = true,
          },

          suggestion = { enabled = false },
          panel = { enabled = false },
        },
      },
    },
  },
}
