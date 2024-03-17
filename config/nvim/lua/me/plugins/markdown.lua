---@type LazySpec[]
return {
  {
    'jakewvincent/mkdnflow.nvim',
    --[[ init = function()
      -- prevent mkdnflow from creating a ton of :Mkdn* commands
      vim.api.nvim_set_var('loaded_mkdnflow', true)
    end, ]]
    opts = {
      modules = {
        maps = true,
        cmp = true, -- add a completion source
        yaml = true, -- parse the frontmatter as YAML to configure mkdnflow (just bib files for now)
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
}
