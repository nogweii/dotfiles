---@type LazySpec
return {
  {
    'stevearc/conform.nvim',
    event = { 'BufWritePre' },
    cmd = { 'ConformInfo' },
    keys = {
      {
        'Z=',
        function()
          require('conform').format({ async = true, lsp_fallback = true })
        end,
        desc = 'Format buffer',
      },
    },
    opts = {
      formatters_by_ft = {
        -- stylua: ignore start
        bash      = { 'shfmt' },
        elixir    = { 'mix' },
        go        = { 'gofmt' },
        json      = { 'jq' },
        lua       = { 'stylua' },
        python    = { 'black' },
        -- ruby      = { 'standardrb' },
        sh        = { 'shfmt' },
        terraform = { 'terraform_fmt' },
        toml      = { 'taplo' },
        yaml      = { 'yamlfmt' },
        json5     = { 'formatjson5' },

        -- For all filetypes:
        -- ['*']     = { 'trim_whitespace', 'trim_newlines' },
        -- For any that don't have a config:
        --['_']     = { 'codespell' },
        -- stylua: ignore end
      },

      format_on_save = function(bufnr)
        -- Disable with a global or buffer-local variable
        if vim.b[bufnr].disable_autoformat then
          return
        end
        -- These options will be passed to conform.format()
        return { timeout_ms = 500, lsp_fallback = true }
      end,

      formatters = {
        formatjson5 = {
          command = 'formatjson5',
          args = { '--one_element_lines', '--sort_arrays', '-' },
          stdin = true,
        },
      },
    },
    init = function()
      -- If you want the formatexpr, here is the place to set it
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    end,
  },

  {
    'johmsalas/text-case.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
    config = function()
      require('textcase').setup({})
      require('telescope').load_extension('textcase')
    end,
    keys = {
      '<leader>tc',
      { '<leader>tc.', '<cmd>TextCaseOpenTelescope<CR>', mode = { 'n', 'x' }, desc = 'Telescope' },
    },
  },

  -- Automatic semi-smart indentation settings for a buffer
  {
    'nmac427/guess-indent.nvim',
    opts = {
      auto_cmd = true,
      -- editorconfig is the source of truth
      override_editorconfig = false,
    },
    event = 'AsyncFileLoad',
  },

  {
    'danymat/neogen',
    opts = {
      snippet_engine = 'luasnip',
    },
    keys = {
      { '<leader>tgg', '<cmd>Neogen<CR>', desc = 'Generate doc annotation' },
      { 'ZN', '<cmd>Neogen<CR>', desc = 'Generate doc annotation' },
    },
    cmd = 'Neogen',
  },

  {
    'LudoPinelli/comment-box.nvim',
    opts = {
      comment_style = 'auto',
    },
    keys = {
      -- manage comment boxes
      { '<leader>tbc', '<cmd>CBcatalog<CR>', desc = 'Show catalog' },
      { '<leader>tbd', '<cmd>CBd<CR>', desc = 'Delete box' },
      { '<leader>tby', '<cmd>CBy<CR>', desc = 'Yank contents of box' },

      -- and bind various styles
      { '<leader>tbb', '<cmd>CBccbox<CR>', desc = 'Box Title' },
      { '<leader>tbt', '<cmd>CBllline<CR>', desc = 'Titled Line' },
      { '<leader>tbl', '<cmd>CBline<CR>', desc = 'Simple Line' },
      { '<leader>tbm', '<cmd>CBllbox14<CR>', desc = 'Marked' },
    },
  },
}
