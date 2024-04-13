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
}
