---@type LazySpec
return {

  -- Launch the file manager or new terminal easily from within vim
  { 'justinmk/vim-gtfo' },

  -- And open a URL in a browser
  {
    'sontungexpt/url-open',
    cmd = 'URLOpenUnderCursor',
    event = 'VeryLazy',
    keys = {
      { 'gou', ':URLOpenUnderCursor<cr>', mode = { 'n' }, desc = 'Open the URL under the cursor' },
    },
    opts = {
      open_only_when_cursor_on_url = true,
      highlight_url = {
        all_urls = {
          enabled = false,
        },
        cursor_move = {
          enabled = true,
        },
      },
      deep_pattern = true,

      -- a list of patterns to open custom text as URLs.
      -- inspiration from the official plugin https://github.com/sontungexpt/url-open/blob/main/lua/url-open/modules/patterns.lua
      -- patterns are documented by lua at https://www.lua.org/pil/20.2.html
      extra_patterns = {
        {
          pattern = '([\'"])([^/])/([^/])%1',
          prefix = 'https://github.com/',
          suffix = '',
          file_patterns = { 'nvim/lua/.%S%.lua' },
          excluded_file_patterns = nil,
        },

        {
          pattern = 'gem [\'"]([^%s]*)[\'"]',
          prefix = 'https://rubygems.org/gems/',
          suffix = '',
          file_patterns = { 'Gemfile', 'gems.rb' },
          excluded_file_patterns = nil,
        },

        -- Dockerfile images: unprefix, unnamespaced (the library images, like `ruby:3.2`)
        -- results in: https://hub.docker.com/_/ruby/
        {
          pattern = '^FROM ([^:.]+):',
          prefix = 'https://hub.docker.com/_/',
          suffix = '/',
          file_patterns = { 'Dockerfile%S*', 'Containerfile%S*' },
          excluded_file_patterns = nil,
          extra_condition = function(matched_pattern)
            return not matched_pattern:match('/')
          end,
        },
        -- Dockerfile images: unprefixed but namespaced (like crystallang/crystal)
        -- results in: https://hub.docker.com/r/crystallang/crystal
        {
          pattern = 'FROM ([^:.]+):',
          prefix = 'https://hub.docker.com/r/',
          suffix = '/',
          file_patterns = { 'Dockerfile%S*', 'Containerfile%S*' },
          excluded_file_patterns = nil,
          extra_condition = function(matched_pattern)
            return matched_pattern:match('/')
          end,
        },
      },
    },
  },
}
