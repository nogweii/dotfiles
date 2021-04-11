return require("packer").startup(function(use)
    -- use packer to manage packer
    use "wbthomason/packer.nvim"

    -- Bug fix https://github.com/neovim/neovim/issues/12587
    -- and see the readme: https://github.com/antoinemadec/FixCursorHold.nvim/blob/master/README.md
    use "antoinemadec/FixCursorHold.nvim"

    -- Automatically jump to the project's root directory
    use "airblade/vim-rooter"

    -- a very beautiful tabline
    use { "romgrk/barbar.nvim", requires = "kyazdani42/nvim-web-devicons" }

    -- a pretty file tree on the side
    use { "kyazdani42/nvim-tree.lua",
      requires = "kyazdani42/nvim-web-devicons",
      module = "nvim-tree"
    }

    -- preview colors inline in the editor
    use {"rrethy/vim-hexokinase",
        cond = "vim.fn.executable('hexokinase')",
        setup = function()
            vim.g.Hexokinase_executable_path = vim.fn.exepath("hexokinase")
            vim.g.Hexokinase_highlighters = {'virtual'}
            vim.g.Hexokinase_optInPatterns = {'full_hex', 'triple_hex', 'rgb', 'rgba', 'hsl', 'hsla'}
        end
    }

    use { "nvim-treesitter/nvim-treesitter",
        run = function()
            require('me.treesitter')
            vim.cmd [[:TSUpdate]]
        end,
        config = function()
            require('me.treesitter')
        end
    }
    -- Treesitter compatible rainbow parentheses
    use { "p00f/nvim-ts-rainbow", requires = "nvim-treesitter/nvim-treesitter", after = "nvim-treesitter" }
    -- Dynamically set &commentstring when moving around files with multiple filetypes combined
    use { "JoosepAlviste/nvim-ts-context-commentstring", requires = "nvim-treesitter/nvim-treesitter", after = "nvim-treesitter" }

    -- A package of language support files, like syntax highlighting.
    -- WARN: this has a pretty substantial impact on startup time; treesitter
    -- does a lot of what's missing. It's still a great resource on awesome
    -- vim plugins for a whole host of file types!
    -- use { "sheerun/vim-polyglot" }

    -- Add TICKscript (Influx Kapacitor 1.x) syntax
    use "nathanielc/vim-tickscript"

    -- Automatically configure various editor settings in a standard way
    use { "editorconfig/editorconfig-vim",
        setup = function()
            if vim.fn.executable('editorconfig') then
                vim.g.EditorConfig_exec_path = vim.fn.exepath("editorconfig")
                vim.g.EditorConfig_core_mode = 'external_command'
            end
        end
    }

    -- Easily put a character/pair around some text. Sandwich a word between
    -- parentheses!
    use { "machakann/vim-sandwich",
      config = function()
        vim.cmd("runtime macros/sandwich/keymap/surround.vim")
      end
    }

    -- Automatically build and maintain a tags file
    use "ludovicchabant/vim-gutentags"

    -- COLORS! All the colors!
    use "glepnir/zephyr-nvim"

    -- A very fast way to open a file, with fuzzy searching!
    use { "wincent/command-t", run = 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make' }

    -- Launch the file manager or new terminal easily from within vim
    use "justinmk/vim-gtfo"

    -- Briefly highlight whatever I yank
    use "machakann/vim-highlightedyank"

    -- Use :StartupTime to get an average of 10 runs of `nvim --startuptime` and
    -- present a nice display of what's taking so long startup. Also, see the shell
    -- alias 'nvim-startup-benchmark'
    use "tweekmonster/startuptime.vim"

    use {"glacambre/firenvim", run = function() vim.fn["firenvim#install"](0) end}

    use "tpope/vim-repeat"
    use "tpope/vim-endwise"
    use "tpope/vim-commentary"
    use "tpope/vim-characterize"
    use "tpope/vim-rsi"
    use "tpope/vim-eunuch"
    use "tpope/vim-fugitive"

    use { "kabouzeid/nvim-lspinstall", requires = "neovim/nvim-lspconfig",
      config = function()
        require('lspinstall').setup()

        local servers = require('lspinstall').installed_servers()
        for _, server in pairs(servers) do
          require('lspconfig')[server].setup{}
        end
      end
    }

    use {"hrsh7th/nvim-compe", event = 'InsertEnter *'}

    use {
      "lewis6991/gitsigns.nvim",
      requires = {
        "nvim-lua/plenary.nvim"
      },
      config = function()
        require('gitsigns').setup()
      end
    }

    use {
      'glepnir/galaxyline.nvim',
      config = function() require('me.statusline') end,
      requires = {'kyazdani42/nvim-web-devicons', opt = true}
    }

    use {
      'monaqa/dial.nvim',
      config = function() require('me.swaps') end
    }

end) -- end of packer's startup / function(use)
