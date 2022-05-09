-- remove the deprecated commands from v1.x
vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])

require("neo-tree").setup({
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
    follow_current_file = true,

    window = {
      mappings = {
        ["gof"] = "open_dir",
        ["got"] = "open_term",
      },
    },

    commands = {
      open_dir = function(state)
        local node = state.tree:get_node()
        local path = node:get_id()
        vim.call("gtfo#open#file", path)
      end,
      open_term = function(state)
        local node = state.tree:get_node()
        local path
        if node.type == "file" then
          path = node:get_parent_id()
        else
          path = node:get_id()
        end
        vim.call("gtfo#open#term", path, "")
      end,
    },
  },
  event_handlers = {
    {
      event = "file_opened",
      handler = function(_)
        -- automatically close Neo-Tree after a file has been opened
        require("neo-tree").close_all()
      end
    },
  }
})
