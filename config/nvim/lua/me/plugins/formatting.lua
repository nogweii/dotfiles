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
        ['*']     = { 'trim_whitespace', 'trim_newlines', 'codespell' },
        -- For any that don't have a config:
        --['_']     = { 'codespell' },
        -- stylua: ignore end
      },
      format_on_save = {
        -- These options will be passed to conform.format()
        timeout_ms = 500,
        lsp_fallback = true,
      },
    },
    init = function()
      -- If you want the formatexpr, here is the place to set it
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    end,
  },
}
