local function nvim_create_augroups(definitions)
  for group_name, definition in pairs(definitions) do
    vim.cmd('augroup ' .. group_name)
    vim.cmd('autocmd!')
    for _, def in ipairs(definition) do
      local command = table.concat(vim.tbl_flatten({ 'autocmd', def }), ' ')
      vim.cmd(command)
    end
    vim.cmd('augroup END')
  end
end

nvim_create_augroups({
  nospell_types = {
    { 'FileType', 'lua', 'setlocal', 'nospell' },
  },

  nvim_tree_changes = {
    { 'FileType', 'NvimTree', 'setlocal', 'cursorline' },
  },

  highlight_yank = { -- Copied from :help lua-highlight
    { 'TextYankPost', '*', 'silent! lua vim.highlight.on_yank {timeout=500}' },
  },

  quickfix_windows = {
    { 'QuickFixCmdPost', 'grep', 'cwindow' },
    { 'QuickFixCmdPost', 'helpgrep', 'cwindow' },
    { 'FileType', 'qf', 'setlocal scrolloff=0 nobuflisted' },
  },

  grepper = {
    {
      'User',
      'Grepper',
      [[call setqflist([], 'r', {'context': {'bqf': {'pattern_hl': histget('/')}}}) | botright copen]],
    },
  },

  postgresql_configs = {
    { 'BufNewFile,BufRead', 'psqlrc,.psqlrc', [[let b:sql_type_override='pgsql' | setfiletype sql]] },
  },

  lsp_document_highlight = {
    { 'CursorMoved', '<buffer>', [[lua vim.lsp.buf.clear_references()]] },
  },
})

-- Ansible file detection & configuration
local au_group_ansiblefilepath = vim.api.nvim_create_augroup('AnsibleFilePath', {})
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufReadPost', 'FileReadPost' }, {
  group = au_group_ansiblefilepath,
  pattern = { '*.yaml', '*.yml' },
  callback = function(au_details)
    local looklike_paths = vim.regex('\\v/(tasks|roles|handlers|(group|host)_vars)/.*\\.ya?ml$')
    local file_base_name = vim.fn.fnamemodify(au_details.file, ':t:r')
    local ansible_file_names = { 'playbook', 'site', 'main', 'requirements' }

    if looklike_paths:match_str(au_details.file) or vim.tbl_contains(ansible_file_names, file_base_name) then
      vim.opt_local.path:append({ './../templates', './../files', 'templates', 'files', '', '.' })
      vim.opt_local.filetype = 'yaml.ansible'
    end
  end,
})

vim.api.nvim_create_autocmd('VimEnter', {
  callback = function()
    local resession = require('resession')
    -- Only load the session if nvim was started with no args
    if vim.fn.argc(-1) == 0 then
      resession.load(vim.fn.getcwd(), { dir = 'dirsession', silence_errors = true })
    end
  end,
})
vim.api.nvim_create_autocmd('VimLeavePre', {
  callback = function()
    local resession = require('resession')
    resession.save(vim.fn.getcwd(), { dir = 'dirsession', notify = false })
  end,
})

-- Whenever an LSP attaches to vim, add the LSP's determined root directory to &path
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if client.workspace_folders ~= nil then
      vim.opt_local.path:append(vim.tbl_map(function(folder)
        return folder.name
      end, client.workspace_folders))
    end
  end,
})

-- Configure the formatter for packer HCL files
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufReadPost', 'FileReadPost' }, {
  pattern = { '*.pkr.hcl' },
  callback = function()
    require('conform').formatters_by_ft.hcl = { 'packer_fmt' }
  end,
})

vim.api.nvim_create_user_command('ConformToggle', function()
  vim.b.disable_autoformat = not vim.b.disable_autoformat
end, {
  desc = 'Toggle automatic formatting-on-save with Conform',
})

-- For these filetypes, disable ufo's fold
vim.api.nvim_create_autocmd({ 'FileType' }, {
  pattern = { 'git', 'help', 'qf', 'fugitive', 'fugitiveblame', 'neo-tree' },
  callback = function()
    vim.b.ufo_provider = ''
  end,
})
