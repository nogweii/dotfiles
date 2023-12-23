---@type LazySpec
return {

  -- Launch the file manager or new terminal easily from within vim
  { "justinmk/vim-gtfo" },

  -- And open a URL in a browser
  {
    "sontungexpt/url-open",
    cmd = "URLOpenUnderCursor",
    event = "VeryLazy",
    keys = {
      { "gou", ":URLOpenUnderCursor<cr>", mode = {"n"}, desc = "Open the URL under the cursor" },
    },
    opts = {
      open_only_when_cursor_on_url = true,
      highlight_url = {
        all_urls = {
          enabled = false,
        },
        cursor_move  = {
          enabled = true,
        }
      },
      deep_pattern = true,

      -- a list of patterns to open custom text as URLs
      extra_patterns = {
        {
        	  pattern = '([\'"])([^/])/([^/])%1',
        	  prefix = "https://github.com/",
        	  suffix = "",
        	  file_patterns = { "nvim/lua/.%S%.lua" },
        	  excluded_file_patterns = nil,
        },

        {
            pattern = 'gem [\'"]([^%s]*)[\'"]',
        	  prefix = "https://rubygems.org/gems/",
        	  suffix = "",
        	  file_patterns = { "Gemfile", "gems.rb" },
        	  excluded_file_patterns = nil,
        },
      }
    },
  },

}
