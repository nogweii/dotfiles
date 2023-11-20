---@type LazySpec
return {
  -- Easily put a character/pair around some text. Sandwich a word between
  -- parentheses!
  {
    "machakann/vim-sandwich",
    config = function()
      vim.cmd("runtime macros/sandwich/keymap/surround.vim")
    end,
  },

  {
    "ribru17/bamboo.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("bamboo").setup({})
      require("bamboo").load()
    end,
  },

  { 'stevearc/resession.nvim', opts = {}, },

  -- Launch the file manager or new terminal easily from within vim
  { "justinmk/vim-gtfo" },

  -- Use :StartupTime to get an average of 10 runs of `nvim --startuptime` and
  -- present a nice display of what's taking so long startup. Also, see the shell
  -- alias 'nvim-startup-benchmark'
  {
    "dstein64/vim-startuptime",
    cmd = "StartupTime",
    init = function()
      vim.g.startuptime_tries = 10
    end,
  },

  { "tpope/vim-characterize" },
  { "tpope/vim-rsi" },

  -- a collection of LSP configs
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Additonal LSP setup for the neovim nvim lua API.
      -- see config/nvim/lua/me/settings/lsp_servers/lua_ls.lua for additional details
      "folke/neodev.nvim",

      -- a downloaded copy of the SchemaStore.org catalog
      -- (used by JSON & YAML LSPs)
      "b0o/schemastore.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
      require("me.settings.lsp")
    end,
    version = false, -- use latest commit rather than version
  },

  {
    "folke/neodev.nvim",
    lazy = true,
    opts = {
      library = {
        enabled = true,
        plugins = true,
      },
      lspconfig = false,
      pathStrict = true,
    }
  },

  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("trouble").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      })
    end,
  },

  -- smart <C-a> and <C-x> that knows how to change dates, enumerated strings, and regular numbers
  {
    "monaqa/dial.nvim",
    keys = {
      { "<C-a>", mode = {"n", "v"}, desc = "Increment or cycle the word under the cursor, smartly" },
      { "<C-x>", mode = {"n", "v"}, desc = "Decrement or cycle the word under the cursor, smartly" },
      { "g<C-a>", mode = {"n", "v"}, desc = "Increment or cycle the word under the cursor, smartly" },
      { "g<C-x>", mode = {"n", "v"}, desc = "Decrement or cycle the word under the cursor, smartly" },
    },
    config = function()
      require("me.settings.dial_swaps")
    end,
  },

  -- when typing `:<number>` scroll to that line, only while in command mode
  -- which allows easy peeking to another location in the file
  {
    "nacro90/numb.nvim",
    config = function()
      require("numb").setup({
        show_numbers = true, -- Enable 'number' for the window while peeking
        show_cursorline = true, -- Enable 'cursorline' for the window while peeking
      })
    end,
  },

  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/popup.nvim", "nvim-lua/plenary.nvim" },
    config = function()
      require("me.settings.telescope")
    end,
  },

  -- Automatic semi-smart indentation settings for a buffer
  {
    "Raimondi/yaifa",
    init = function()
      vim.g.yaifa_shiftwidth = 2
      vim.g.yaifa_tabstop = 4
      vim.g.yaifa_expandtab = 1
    end,
    version = false,
  },

  {
    "windwp/nvim-autopairs",
    config = function()
      require("me.settings.autopairs")
    end,
  },

  -- quickly toggle comments for a line (or motion)
  { "b3nj5m1n/kommentary" },

  { "mfussenegger/nvim-lint" },

  -- A smarter cursor position restoration function, excluding various buffers
  -- where it makes sense, and opening folds if needed.
  { "farmergreg/vim-lastplace",
    init = function()
      vim.g.lastplace_ignore_buftype = "quickfix,nofile,help"
      vim.g.lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
      vim.g.lastplace_open_folds = 1
    end
  },

  -- Key bindings help & reminder
  {
    "folke/which-key.nvim",
    config = function()
      require("me.settings.which_key")
    end,
  },

  -- Asynchronously use search tools
  {
    "mhinz/vim-grepper",
    cmd = "Grepper",
    init = function ()
      vim.g.grepper = {
        open = 0,
        quickfix = 1,
        searchreg = 1,
        highlight = 0,
      }
    end
  },

  -- snippets engine
  {
    "L3MON4D3/LuaSnip",
    dependencies = { "rafamadriz/friendly-snippets" },
    config = function()
      require("me.settings.luasnip")
    end,
  },
  -- a bunch of community maintained snippets
  { "rafamadriz/friendly-snippets" },

  -- advanced & flexible completion menu
  {
    "hrsh7th/nvim-cmp",
    config = function()
      require("me.settings.cmp")
    end,
    dependencies = { "L3MON4D3/LuaSnip" },
  },
  -- additional sources for cmp, lazily loaded
  { "saadparwaiz1/cmp_luasnip" },
  { "hrsh7th/cmp-nvim-lsp" },
  { "hrsh7th/cmp-buffer" },
  { "hrsh7th/cmp-path" },

  -- Support HCL and other Hashicorp specific syntaxes
  { "hashivim/vim-terraform" },

  -- addtional syntax highlighting for postgresql extensions
  {
    "lifepillar/pgsql.vim",
    ft = "sql",
  },

  {
    "lukas-reineke/headlines.nvim",
    opts = function()
      local opts = {}
      for _, ft in ipairs({ "markdown", "norg", "rmd", "org" }) do
        opts[ft] = {
          headline_highlights = {},
        }
        for i = 1, 6 do
          local hl = "Headline" .. i
          vim.api.nvim_set_hl(0, hl, { link = "Headline", default = true })
          table.insert(opts[ft].headline_highlights, hl)
        end
      end
      return opts
    end,
    ft = { "markdown", "norg", "rmd", "org" },
    config = function(_, opts)
      -- PERF: schedule to prevent headlines slowing down opening a file
      vim.schedule(function()
        require("headlines").setup(opts)
        require("headlines").refresh()
      end)
    end,
  },

  -- Add TICKscript (Influx Kapacitor 1.x) syntax
  { "nathanielc/vim-tickscript" },

  -- quickly & easily generate a python docstring
  {
    "heavenshell/vim-pydocstring",
    build = "make install",
    ft = "python",
    config = function()
      vim.g.pydocstring_formatter = "google"
      vim.g.pydocstring_enable_mapping = 0
      local cmd_map = require("me.map_utils").cmd_map
      cmd_map({ keys = "<leader>pd", command = "Pydocstring" })
    end,
  },

  -- Some utility key bindings for editng markdown tables
  {
    "allen-mack/nvim-table-md",
    ft = "markdown",
    config = function()
      vim.keymap.set("n", "<leader>mto", function()
        require("tablemd").insertRow(false)
      end)
      vim.keymap.set("n", "<leader>mtO", function()
        require("tablemd").insertRow(true)
      end)
      vim.keymap.set("n", "<leader>mti", function()
        require("tablemd").insertColumn(true)
      end)
      vim.keymap.set("n", "<leader>mtI", function()
        require("tablemd").insertColumn(false)
      end)
      vim.keymap.set("n", "<leader>mtf", function()
        require("tablemd").format()
      end)
      vim.keymap.set("n", "<leader>mtd", function()
        require("tablemd").deleteColumn()
      end)

      local wk = require("which-key")
      wk.register({
        name = "Markdown Table",
        o = "Add a new row below",
        O = "Add a new row above",
        i = "Add a new column to the right",
        I = "Add a new column to the left",
        f = "Reformat the table",
        d = "Delete the current column",
      }, { prefix = "<leader>mt" })
    end,
  },

  { "jiaoshijie/undotree", cmd = "UndotreeToggle" },

  {
    "mhartington/formatter.nvim",
    config = function()
      require("me.settings.formatter")
    end,
  },

  -- ReasonML & ReScript syntax support
  { "amiralies/vim-reason" },
  { "rescript-lang/vim-rescript" },

  { "Joorem/vim-haproxy" },

  {
    "echasnovski/mini.align",
    version = "*",
    config = function()
      require("mini.align").setup({
        mappings = {
          start = "", -- ga is already mapped to show the Unicode character, so don't overwrite that
          start_with_preview = "gA",
        },
      })
    end,
  },
}