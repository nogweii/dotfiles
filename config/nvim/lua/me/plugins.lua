-- Bail out early if I haven't ran my `nvim-packs` shell function yet
_G.packer_exists = pcall(vim.cmd, 'packadd packer.nvim')
if not packer_exists then
  return nil
end

return require("packer").startup {
  function(use)
    -- use packer to manage packer
    use { "wbthomason/packer.nvim" }

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
      as = "nvim-tree"
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
        require('me.settings.treesitter')
        vim.cmd [[:TSUpdate]]
      end,
      config = function()
        require('me.settings.treesitter')
      end,
      event = 'BufRead',
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
    use { "glepnir/zephyr-nvim",
      config = function()
        vim.cmd [[colorscheme zephyr]]
      end
    }

    -- A very fast way to open a file, with fuzzy searching!
    use { "wincent/command-t", run = 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make' }

    -- Launch the file manager or new terminal easily from within vim
    use "justinmk/vim-gtfo"

    -- Use :StartupTime to get an average of 10 runs of `nvim --startuptime` and
    -- present a nice display of what's taking so long startup. Also, see the shell
    -- alias 'nvim-startup-benchmark'
    use {'tweekmonster/startuptime.vim', cmd = 'StartupTime'}

    -- run neovim in my browser!
    use {"glacambre/firenvim", run = function() vim.fn["firenvim#install"](0) end}

    use "tpope/vim-repeat"
    use "tpope/vim-characterize"
    use "tpope/vim-eunuch"
    use "tpope/vim-fugitive"

    -- a collection of lsp server installation scripts
    use "neovim/nvim-lspconfig"
    use { "kabouzeid/nvim-lspinstall", requires = "neovim/nvim-lspconfig",
      config = function() require('me.settings.lsp') end
    }
    use { "onsails/lspkind-nvim", requires = "neovim/nvim-lspconfig" }

    use {"hrsh7th/nvim-compe", event = 'InsertEnter *', config = [[require('me.settings.compe')]]}

    -- put git change information in the sidebar, provide some helpers
    -- to manage git hunks
    use {
      "lewis6991/gitsigns.nvim",
      requires = {
        "nvim-lua/plenary.nvim"
      },
      config = function()
        require('gitsigns').setup {
          keymaps = { }, -- empty keymaps list to disable the default ones
        }
      end
    }
    use {'rhysd/git-messenger.vim', cmd = 'GitMessenger'}
    use {
      'ruifm/gitlinker.nvim',
      requires = 'nvim-lua/plenary.nvim',
      config = function()
        require("gitlinker").setup {
          opts = {
            -- mappings = nil,
            add_current_line_on_normal_mode = false,
            action_callback = require("gitlinker.actions").copy_to_clipboard,
          },
          callbacks = {
            ["code.aether.earth"] = require("gitlinker.hosts").get_gitlab_type_url
          }
        }
      end
    }

    -- asynchronous status bar and framework for full customization
    use {
      'glepnir/galaxyline.nvim',
      config = function() require('me.settings.galaxyline') end,
      requires = {'kyazdani42/nvim-web-devicons', opt = true}
    }

    -- smart <C-a> and <C-x> that knows how to change dates, enumerated strings, and regular numbers
    use {
      'monaqa/dial.nvim',
      config = function() require('me.settings.dial_swaps') end
    }

    -- when typing `:<number>` scroll to that line, only while in command mode
    -- which allows easy peeking to another location in the file
    use { 'nacro90/numb.nvim',
      config = function()
        require('numb').setup{
          show_numbers = true, -- Enable 'number' for the window while peeking
          show_cursorline = true -- Enable 'cursorline' for the window while peeking
        }
      end
    }

    -- Automatic semi-smart indentation settings for a buffer
    use {
      'Raimondi/yaifa',
      config = function()
        vim.g.yaifa_shiftwidth = 2
        vim.g.yaifa_tabstop = 4
        vim.g.yaifa_expandtab = 1
      end
    }

    -- automatically add closing pair characters ({}, <>, quotes, and more)
    use { 'Raimondi/delimitMate' }

    -- my preferred snippet engine for vim
    use { 'SirVer/ultisnips' }
    -- community-maintained snippets for a variety of languages
    use { 'honza/vim-snippets', requires = 'SirVer/ultisnips' }

    -- quickly toggle comments for a line (or motion)
    use "b3nj5m1n/kommentary"

    use { "dense-analysis/ale",
      setup = function() require('me.settings.ale') end
    }
    use { "nathunsmitty/nvim-ale-diagnostic", requires = "dense-analysis/ale", module = "nvim-ale-diagnostic" }

    use "hashivim/vim-terraform"

  end, -- end of function(use)

  config = {
    profile = {
      enable = true,
      threshold = 1
    },
    display = {
      -- instead of opening a new window to the side, open a floating one
      open_fn = require('packer.util').float,
    },
  }
} -- end of packer's setup()
