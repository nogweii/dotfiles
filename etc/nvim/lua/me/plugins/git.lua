---@type LazySpec
return {
  -- get more recently updated git related syntax files
  -- this is the upstream source of what is shipped with the editor
  { 'tpope/vim-git' },

  -- put git change information in the sidebar, provide some helpers
  -- to manage git hunks
  {
    'lewis6991/gitsigns.nvim',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      'nvim-lua/plenary.nvim',
    },
    config = function()
      require('gitsigns').setup({})
    end,
  },

  -- show git blame in a popup
  { 'rhysd/git-messenger.vim', cmd = 'GitMessenger' },

  -- yank a link to the commit
  {
    'linrongbin16/gitlinker.nvim',
    opts = function(_plugin, _opts)
      return {
        router = {
          browse = {
            ['^code%.aether%.earth'] = require('gitlinker.routers').gitlab_browse,
          },
          blame = {
            ['^code%.aether%.earth'] = require('gitlinker.routers').gitlab_blame,
          },
        },
      }
    end,
    keys = {
      {
        '<leader>gy',
        function()
          require('gitlinker').link({
            action = require('gitlinker.actions').clipboard,
            router_type = 'browse',
          })
        end,
        mode = { 'n', 'v' },
        desc = 'Yank a git URL to this file',
      },
      {
        '<leader>gY',
        function()
          require('gitlinker').link({
            action = require('gitlinker.actions').system,
            router_type = 'browse',
          })
        end,
        mode = { 'n', 'v' },
        desc = 'Open the git URL in browser',
      },
    },
  },

  -- Easily add co-authors to commits using the typical "Co-authored-by:" signature
  {
    '2kabhishek/co-author.nvim',
    dependencies = {
      'stevearc/dressing.nvim',
      'nvim-telescope/telescope.nvim',
    },
    cmd = { 'CoAuthor' },
    keys = {
      { '<leader>gC', '<cmd>CoAuthor<CR>', mode = { 'n' }, desc = 'Add co-authors to the commit' },
    },
  },
}
