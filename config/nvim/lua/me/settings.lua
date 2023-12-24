-- Speed up loading vim remote plugin hosts. Since they do a more complicated
-- heuristic than my systems need, I can shortcut the work needed.
if vim.fn.executable('neovim-ruby-host') then
  vim.g.ruby_host_prog = vim.fn.exepath('neovim-ruby-host')
end

-- use homebrew python if it exists (macOS support)
if vim.fn.filereadable('/opt/homebrew/bin/python3') == 1 then
  vim.g.python3_host_prog = '/opt/homebrew/bin/python3'
else
  -- otherwise presume the system-wide python3 is correct (which it is for Arch)
  vim.g.python3_host_prog = '/usr/bin/python3'
end
vim.g.loaded_python_provider = 0
