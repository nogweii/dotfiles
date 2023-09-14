local plugins = {
  -- use packer to manage packer
  { "wbthomason/packer.nvim" },

  -- Bug fix https://github.com/neovim/neovim/issues/12587
  -- and see the readme: https://github.com/antoinemadec/FixCursorHold.nvim/blob/master/README.md
  { "antoinemadec/FixCursorHold.nvim" },

  -- a few performance optimiziation tweaks to neovim.
  -- remove once neovim core gets something similar: https://github.com/neovim/neovim/pull/15436
  { "lewis6991/impatient.nvim" },

  -- Automatically jump to the project's root directory
  { "airblade/vim-rooter" },

  -- a very beautiful tabline
  { "romgrk/barbar.nvim", dependencies = { "kyazdani42/nvim-web-devicons" } },

  -- a pretty file tree on the side
  {
    "nvim-neo-tree/neo-tree.nvim",
    -- branch = "v2.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "kyazdani42/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
    config = function()
      require("me.settings.neo-tree")
    end,
  },

  -- preview colors inline in the editor
  {
    "NvChad/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup()
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require("me.settings.treesitter")
    end,
    event = "BufRead",
  },
  -- Treesitter compatible rainbow parentheses
  { "p00f/nvim-ts-rainbow", dependencies = { "nvim-treesitter/nvim-treesitter" } },
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

  -- Automatically configure various editor settings in a standard way
  {
    "editorconfig/editorconfig-vim",
    init = function()
      if vim.fn.executable("editorconfig") then
        vim.g.EditorConfig_exec_path = vim.fn.exepath("editorconfig")
        vim.g.EditorConfig_core_mode = "external_command"
      end
    end,
  },

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
    config = function()
      require("bamboo").setup({})
      require("bamboo").load()
    end,
  },

  -- Launch the file manager or new terminal easily from within vim
  { "justinmk/vim-gtfo" },

  -- Use :StartupTime to get an average of 10 runs of `nvim --startuptime` and
  -- present a nice display of what's taking so long startup. Also, see the shell
  -- alias 'nvim-startup-benchmark'
  { "tweekmonster/startuptime.vim", cmd = "StartupTime" },

  { "tpope/vim-repeat" },
  { "tpope/vim-characterize" },
  { "tpope/vim-eunuch" },
  { "tpope/vim-fugitive" },
  -- get more recently updated git related syntax files
  -- this is the upstream source of what is shipped with the editor
  { "tpope/vim-git" },
  { "tpope/vim-rsi" },

  -- a collection of LSP configs
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Additonal LSP setup for the neovim nvim lua API.
      -- see config/nvim/lua/me/settings/lsp_servers/sumneko_lua.lua for additional details
      "folke/neodev.nvim",

      -- a downloaded copy of the SchemaStore.org catalog
      -- (used by JSON & YAML LSPs)
      "b0o/schemastore.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
      require("me.settings.lsp")
    end,
  },

  {
    "folke/trouble.nvim",
    dependencies = { "kyazdani42/nvim-web-devicons" },
    config = function()
      require("trouble").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      })
    end,
  },

  -- put git change information in the sidebar, provide some helpers
  -- to manage git hunks
  {
    "lewis6991/gitsigns.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("gitsigns").setup({})
    end,
  },
  -- show git blame in a popup
  { "rhysd/git-messenger.vim", cmd = "GitMessenger" },
  -- yank a link to the commit
  {
    "ruifm/gitlinker.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("gitlinker").setup({
        opts = {
          add_current_line_on_normal_mode = false,
          action_callback = require("gitlinker.actions").copy_to_clipboard,
        },
        callbacks = {
          ["code.aether.earth"] = require("gitlinker.hosts").get_gitlab_type_url,
        },
        mappings = nil,
      })
    end,
  },

  -- a very customizble status bar framework for Neovim written in Lua
  {
    "feline-nvim/feline.nvim",
    config = function()
      require("me.settings.feline")
    end,
    dependencies = { "kyazdani42/nvim-web-devicons", "lewis6991/gitsigns.nvim" },
  },

  -- smart <C-a> and <C-x> that knows how to change dates, enumerated strings, and regular numbers
  {
    "monaqa/dial.nvim",
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
  { "farmergreg/vim-lastplace" },

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
  },

  {
    "kevinhwang91/nvim-bqf",
    config = function()
      require("bqf").setup()
    end,
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

  -- Better markdown syntax
  {
    "plasticboy/vim-markdown",
    ft = "markdown",
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
}

-- Bootstrap lazy.nvim by automatically cloning the git repo
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  if not vim.fn.executable("git") then
    print("Lazy.nvim not installed and git not found in PATH. Plugins aren't available!")
  else
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable", -- latest stable release
      lazypath,
    })
    print("Lazy.nvim git repository cloned.")
  end
end
vim.opt.rtp:prepend(lazypath)

local opts = {
  ui = {
    icons = {
      cmd = "⌘",
      config = "🛠",
      event = "📅",
      ft = "📂",
      init = "⚙",
      keys = "🗝",
      plugin = "🔌",
      runtime = "💻",
      source = "📄",
      start = "🚀",
      task = "📌",
      lazy = "💤 ",
    },
  },

  defaults = {
    version = "*",
  },
}

require("lazy").setup(plugins, opts)
