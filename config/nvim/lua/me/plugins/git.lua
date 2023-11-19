return {
  { "tpope/vim-fugitive" },
  -- get more recently updated git related syntax files
  -- this is the upstream source of what is shipped with the editor
  { "tpope/vim-git" },

  -- put git change information in the sidebar, provide some helpers
  -- to manage git hunks
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
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

}
