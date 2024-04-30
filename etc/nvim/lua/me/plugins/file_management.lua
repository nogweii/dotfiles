---@type LazySpec
return {
  {
    'tpope/vim-eunuch',
    cmd = { 'Chmod', 'Delete', 'Edit', 'Grep', 'Mkdir', 'Move', 'Rename', 'Unlink', 'Wall', 'Write' },
  },

  -- Automatically jump to the project's root directory
  {
    'ahmedkhalf/project.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
    config = function()
      require('project_nvim').setup({
        patterns = { '!>packages', '>code', '.git', '_darcs', '.hg', '.bzr', '.svn', 'Makefile', 'package.json' },
        -- try matching a pattern first, *then* use the LSP server's idea
        detection_methods = { 'pattern', 'lsp' },
      })
      require('telescope').load_extension('projects')
    end,
  },

  -- a pretty file tree on the side
  {
    'nvim-neo-tree/neo-tree.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-tree/nvim-web-devicons',
      'MunifTanjim/nui.nvim',
    },
    opts = {
      close_if_last_window = true,
      enable_git_status = true,
      enable_diagnostics = true,
      sort_case_insensitive = true,
      default_component_configs = {
        modified = {
          symbol = '💾',
        },
        name = {
          trailing_slash = true,
          use_git_status_colors = true,
        },
      },
      filesystem = {
        filtered_items = {
          -- don't actually hide them, keep them visible. they will be styled differently
          visible = true,
          hide_dotfiles = true,
          hide_gitignored = true,
        },
        follow_current_file = {
          enabled = true,
        },

        window = {
          mappings = {
            ['gof'] = 'open_dir',
            ['got'] = 'open_term',
            y = 'yank_rel_path',
            Y = 'yank_abs_path',
            c = 'copy_to_clipboard',
          },
        },

        commands = {
          open_dir = function(state)
            local node = state.tree:get_node()
            local path = node:get_id()
            vim.call('gtfo#open#file', path)
          end,
          open_term = function(state)
            local node = state.tree:get_node()
            local path
            if node.type == 'file' then
              path = node:get_parent_id()
            else
              path = node:get_id()
            end
            vim.call('gtfo#open#term', path, '')
          end,

          yank_rel_path = function(state)
            local node = state.tree:get_node()
            local path = vim.fn.fnamemodify(node:get_id(), ':.')
            vim.notify('Copied: ' .. path)
            vim.fn.setreg('+', path)
          end,

          yank_abs_path = function(state)
            local node = state.tree:get_node()
            local path = node:get_id()
            vim.notify('Copied: ' .. path)
            vim.fn.setreg('+', path)
          end,
        },
      },
      event_handlers = {
        {
          event = 'file_opened',
          handler = function(_)
            -- automatically close Neo-Tree after a file has been opened
            require('neo-tree').close_all()
          end,
        },
      },
    },
    version = '*',
    cmd = 'Neotree',
  },

  {
    'antosha417/nvim-lsp-file-operations',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-neo-tree/neo-tree.nvim',
    },
    opts = {},
  },
}
