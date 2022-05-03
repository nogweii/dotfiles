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

    -- a few performance optimiziation tweaks to neovim.
    -- remove once neovim core gets something similar: https://github.com/neovim/neovim/pull/15436
    use 'lewis6991/impatient.nvim'

    -- Automatically jump to the project's root directory
    use "airblade/vim-rooter"

    -- a very beautiful tabline
    use { "romgrk/barbar.nvim", requires = "kyazdani42/nvim-web-devicons" }

    -- a pretty file tree on the side
    use { "kyazdani42/nvim-tree.lua",
      requires = "kyazdani42/nvim-web-devicons",
      as = "nvim-tree",
      config = function()
        require('nvim-tree').setup {
          update_cwd = true,
          update_focused_file = {
            enable = true,
            update_cwd = true
          }
        }
      end,
    }

    -- preview colors inline in the editor
    use {"rrethy/vim-hexokinase",
      setup = function()
        if vim.fn.executable('hexokinase') == 1 then
          vim.g.Hexokinase_executable_path = vim.fn.exepath("hexokinase")
        end
        vim.g.Hexokinase_highlighters = {'virtual'}
        vim.g.Hexokinase_optInPatterns = {'full_hex', 'triple_hex', 'rgb', 'rgba', 'hsl', 'hsla'}
      end
    }

    use { "nvim-treesitter/nvim-treesitter",
      run = ':TSUpdate',
      config = function()
        require('me.settings.treesitter')
      end,
      event = 'BufRead',
    }
    -- Treesitter compatible rainbow parentheses
    use { "p00f/nvim-ts-rainbow", requires = "nvim-treesitter/nvim-treesitter", after = "nvim-treesitter" }
    -- Dynamically set &commentstring when moving around files with multiple filetypes combined
    use { "JoosepAlviste/nvim-ts-context-commentstring", requires = "nvim-treesitter/nvim-treesitter", after = "nvim-treesitter" }
    -- make neovim's spell checking treesitter aware
    use {
      "lewis6991/spellsitter.nvim",
      config = function()
        require('spellsitter').setup({
          enabled = true,
          spellchecker = 'ffi'
        })
      end,
      requires = "nvim-treesitter/nvim-treesitter",
      after = "nvim-treesitter",
    }

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

    -- COLORS! All the colors!
    use { "catppuccin/nvim",
      as = "catppuccin"
      -- config = [[require('me.settings.colors.catppuccin')]]
    }
    use { "marko-cerovac/material.nvim",
      config = [[require('me.settings.colors.material')]]
    }
    -- TODO: evaluate these
    use 'folke/tokyonight.nvim'
    use 'EdenEast/nightfox.nvim'

    -- Launch the file manager or new terminal easily from within vim
    use "justinmk/vim-gtfo"

    -- Use :StartupTime to get an average of 10 runs of `nvim --startuptime` and
    -- present a nice display of what's taking so long startup. Also, see the shell
    -- alias 'nvim-startup-benchmark'
    use {'tweekmonster/startuptime.vim', cmd = 'StartupTime'}

    use "tpope/vim-repeat"
    use "tpope/vim-characterize"
    use "tpope/vim-eunuch"
    use "tpope/vim-fugitive"
    -- get more recently updated git related syntax files
    -- this is the upstream source of what is shipped with the editor
    use { 'tpope/vim-git' }

    -- easily install LSP servers in isolation from the rest of the system
    use { "williamboman/nvim-lsp-installer",
        requires = {
          -- a collection of LSP configs
          "neovim/nvim-lspconfig",

          -- Additonal LSP setup for the neovim nvim lua API.
          -- see config/nvim/lua/me/settings/lsp_servers/sumneko_lua.lua for additional details
          "folke/lua-dev.nvim"
        },
        config = function() require('me.settings.lsp') end,
        after = "cmp-nvim-lsp"
    }

    use {
      "folke/trouble.nvim",
      requires = "kyazdani42/nvim-web-devicons",
      config = function()
        require("trouble").setup {
          -- your configuration comes here
          -- or leave it empty to use the default settings
          -- refer to the configuration section below
        }
      end
    }

    -- insert completion menu
    --use {"hrsh7th/nvim-compe", event = 'InsertEnter *', config = [[require('me.settings.compe')]]}

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
    -- show git blame in a popup
    use {'rhysd/git-messenger.vim', cmd = 'GitMessenger'}
    -- yank a link to the commit
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

    -- a very customizble status bar framework for Neovim written in Lua
    use { 'feline-nvim/feline.nvim',
      tag = '*',
      config = function() require('me.settings.feline') end,
      requires = {'kyazdani42/nvim-web-devicons', 'lewis6991/gitsigns.nvim'}
    }

    -- smart <C-a> and <C-x> that knows how to change dates, enumerated strings, and regular numbers
    use {
      'monaqa/dial.nvim',
      tag = '*',
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

    use { 'nvim-telescope/telescope.nvim',
      requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
      config = function() require('me.settings.telescope') end
    }

    -- Automatic semi-smart indentation settings for a buffer
    use {
      'Raimondi/yaifa',
      setup = function()
        vim.g.yaifa_shiftwidth = 2
        vim.g.yaifa_tabstop = 4
        vim.g.yaifa_expandtab = 1
      end
    }

    -- automatically add closing pair characters ({}, <>, quotes, and more)
    use { 'Raimondi/delimitMate' }

    -- quickly toggle comments for a line (or motion)
    use "b3nj5m1n/kommentary"

    use 'mfussenegger/nvim-lint'

    -- A smarter cursor position restoration function, excluding various buffers
    -- where it makes sense, and opening folds if needed.
    use "farmergreg/vim-lastplace"

    -- Key bindings help & reminder
    use {
      "folke/which-key.nvim",
      config = function() require('me.settings.which_key') end
    }

    -- Asynchronously use search tools
    use { 'mhinz/vim-grepper',
      cmd = 'Grepper',
      keys = '<plug>(GrepperOperator)',
    }

    use { 'kevinhwang91/nvim-bqf',
      config = function()
        require('bqf').setup({
          preview = {
            win_height = 10,
            win_vheight = 10,
            delay_syntax = 100,
            border_chars = {'┃', '┃', '━', '━', '┏', '┓', '┗', '┛', '█'}
          },
          func_map = {
            vsplit = '',
            ptogglemode = 'z,',
            stoggleup = ''
          },
          --[[ filter = {
            fzf = {
              action_for = {['ctrl-s'] = 'split'},
              extra_opts = {'--bind', 'ctrl-o:toggle-all', '--prompt', '> '}
            }
          } ]]
        })
      end
    }

    -- snippets engine
    use { "L3MON4D3/LuaSnip",
      after = "friendly-snippets",
      config = function()
        require('me.settings.luasnip')
      end
    }
    -- a bunch of community maintained snippets
    use { "rafamadriz/friendly-snippets",
      -- lazily loaded when either of these happen:
      -- event = "InsertEnter"
    }

    -- advanced & flexible completion menu
    use { "hrsh7th/nvim-cmp",
      config = function()
        require('me.settings.cmp')
      end,
      after = "LuaSnip"
    }
    -- additional sources for cmp, lazily loaded
    use { "saadparwaiz1/cmp_luasnip",
      after = "nvim-cmp",
    }
    use { "hrsh7th/cmp-nvim-lsp",
      after = "nvim-cmp",
    }
    use { "hrsh7th/cmp-buffer",
      after = "nvim-cmp",
    }
    use { "hrsh7th/cmp-path",
      after = "nvim-cmp",
    }

    -- Support HCL and other Hashicorp specific syntaxes
    use "hashivim/vim-terraform"

    -- addtional syntax highlighting for postgresql extensions
    use { 'lifepillar/pgsql.vim',
      ft = 'sql'
    }

    -- Better markdown syntax
    use {
      'plasticboy/vim-markdown',
      ft = 'markdown'
    }

    -- Add TICKscript (Influx Kapacitor 1.x) syntax
    use "nathanielc/vim-tickscript"

    -- quickly & easily generate a python docstring
    use { 'heavenshell/vim-pydocstring',
      run = 'make install',
      ft = 'python',
      config = function()
        vim.g.pydocstring_formatter = 'google'
        vim.g.pydocstring_enable_mapping = 0
        local cmd_map = require('me.map_utils').cmd_map
        cmd_map{keys = "<leader>pd", command = "Pydocstring"}
      end
    }

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
    -- move the compiled file to a different location so that it must be loaded
    -- by require(), allowing impatient.nvim to work its magic
    compile_path = vim.fn.stdpath('config') .. '/lua/packer_compiled.lua'
  }
} -- end of packer's setup()
