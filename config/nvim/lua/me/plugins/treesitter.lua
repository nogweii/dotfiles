return {

  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      vim.g.skip_ts_context_commentstring_module = true

      require("nvim-treesitter.configs").setup({
        -- Enable some modules shipped with nvim-treesitter
        highlight = {
          enable = true,
        },
        indent = {
          enable = true,
        },
      })

      require('ts_context_commentstring').setup {}
    end,
    event = "BufRead",
  },

  -- Treesitter compatible rainbow parentheses
  { "HiPhish/rainbow-delimiters.nvim", dependencies = { "nvim-treesitter/nvim-treesitter" } },

  -- Dynamically set &commentstring when moving around files with multiple filetypes combined
  { "JoosepAlviste/nvim-ts-context-commentstring", dependencies = { "nvim-treesitter/nvim-treesitter" } },

  -- Add some context to where I am in a file
  {
    "nvim-treesitter/nvim-treesitter-context",
    config = function()
      require("treesitter-context").setup({})
    end,
    dependencies = { "nvim-treesitter/nvim-treesitter" },
  },

}
