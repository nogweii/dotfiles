---@type LazySpec[]
return {
  {
    'jakewvincent/mkdnflow.nvim',
    opts = {
      modules = {
        maps = false, -- don't do keymaps, ftplugin/markdown.lua has it handled
        cmp = true,
        yaml = true, -- parse the frontmatter as YAML to configure mkdnflow (just bib files for now)
      },
      links = {
        style = 'markdown',
      },
      perspective = {
        priority = 'root', -- search for files relative to the root of the notebook/wiki
        root_tell = '.marksman.toml', -- this file is where the root is at
        nvim_wd_heel = true, -- change nvim's dir when navigating files
      },
    },
  },

  {
    'lukas-reineke/headlines.nvim',
    opts = function()
      local opts = {}
      for _, ft in ipairs({ 'markdown', 'norg', 'rmd', 'org' }) do
        opts[ft] = {
          headline_highlights = {},
        }
        for i = 1, 6 do
          local hl = 'Headline' .. i
          vim.api.nvim_set_hl(0, hl, { link = 'Headline', default = true })
          table.insert(opts[ft].headline_highlights, hl)
        end
      end
      return opts
    end,
    ft = { 'markdown', 'norg', 'rmd', 'org' },
    config = function(_, opts)
      -- PERF: schedule to prevent headlines slowing down opening a file
      vim.schedule(function()
        require('headlines').setup(opts)
        require('headlines').refresh()
      end)
    end,
  },

  -- Some utility key bindings for editng markdown tables
  {
    'allen-mack/nvim-table-md',
    ft = 'markdown',
    keys = {
      {
        '<leader>mto',
        function()
          require('tablemd').insertRow(false)
        end,
        desc = 'Add table row below',
      },
      {
        '<leader>mtO',
        function()
          require('tablemd').insertRow(true)
        end,
        desc = 'Add table row above',
      },
      {
        '<leader>mti',
        function()
          require('tablemd').insertColumn(true)
        end,
        desc = 'Add table column to the right',
      },
      {
        '<leader>mtI',
        function()
          require('tablemd').insertColumn(false)
        end,
        desc = 'Add table column to the left',
      },
      {
        '<leader>mtf',
        function()
          require('tablemd').format()
        end,
        desc = 'Reformat the table',
      },
      {
        '<leader>mtd',
        function()
          require('tablemd').deleteColumn()
        end,
        desc = 'Delete current table column',
      },
    },
  },
}
