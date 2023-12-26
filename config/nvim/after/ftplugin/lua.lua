vim.opt_local.include = [[\v<((do|load)file|require)[^''"]*[''"]\zs[^''"]+]]

-- if there is anything in the path that has 'vim' in it,
-- add all of the paths in &rtp as search targets
if vim.api.nvim_buf_get_name(0):find('vim') then
  for _, path in pairs(vim.api.nvim_list_runtime_paths()) do
    vim.opt_local.path:append(path .. '/lua')
  end
end

vim.opt_local.suffixesadd:prepend('init.lua')
vim.opt_local.suffixesadd:prepend('.lua')
