-- Check if there is more than a single directory that can find Treesitter
-- queries. Specifically, query matching these two rules:
--
--  1. Something that is shipped as part of the core nvim-treesitter plugin.
--  2. A language that is not shipped with neovim itself.
--
-- Thus looking for queries for highlighting ruby code.

local system_rtp_dir = '/usr/lib/nvim'

if not vim.o.runtimepath:find(system_rtp_dir) then
  return
end

-- The particular choice of Ruby is because I like it, not that the ruby
-- highlights file is special
local ruby_queries = vim.api.nvim_get_runtime_file('queries/ruby/highlights.scm', true)

if #ruby_queries > 1 then
  -- There is more than 1 directory that has it. Almost certainly this means I'm running on
  -- a system that has installed the nvim-treesitter-parsers-git pacman package.
  -- With nvim-treesitter, the queries & parsers are supposed to be managed together. If either
  -- get out of sync with the other, various errors start occurring.

  -- When searching nvim's &rtp, it will use the first found file, so here I intentionally move
  -- the directory to the beginning of the search path.
  vim.opt.runtimepath:remove(system_rtp_dir)
  vim.opt.runtimepath:prepend(system_rtp_dir)
end
