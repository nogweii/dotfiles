---@type LazySpec[]
return {

  {
    "williamboman/mason.nvim",
    opts = {
      ui = {
        icons = {
          package_installed = "✅",
          package_uninstalled = "❌",
          package_pending = "🔁",
        },
      },
    },
    build = ":MasonUpdate",
    lazy = false,
  },

  {
    "jay-babu/mason-null-ls.nvim",
    dependencies = {
      "williamboman/mason.nvim",
      "nvimtools/none-ls.nvim",
      "nvimtools/none-ls-extras.nvim",
    },
    opts = {
      ensure_installed = {},
      automatic_installation = true,
      handlers = {}, -- by setting this to {} all installed tools will be registered with none-ls
    },
    lazy = false,
  },

  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = {
      "williamboman/mason.nvim",
      'neovim/nvim-lspconfig',
    },
    lazy = false,
    opts = {
      ensure_installed = {},
    }
  },

  -- {
  --   "jay-babu/mason-nvim-dap.nvim",
  -- },

  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "jay-babu/mason-null-ls.nvim",
    },
    lazy = false,
    opts = {
      -- ensure_installed = {},
      integrations = {
        ["mason-lspconfig"] = true,
        ["mason-null-ls"] = true,
        ["mason-nvim-dap"] = false, -- TODO: add debugging
      }
    }
  },

}
