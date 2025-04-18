-- nvim_create_augroups({
-- nospell_types = {
--   { 'FileType', 'lua', 'setlocal', 'nospell' },
-- },
-- lsp_document_highlight = {
--   { 'CursorMoved', '<buffer>', [[lua vim.lsp.buf.clear_references()]] },
-- },
-- })

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

-- For these filetypes, disable ufo's fold
vim.api.nvim_create_autocmd({ 'FileType' }, {
  pattern = { 'git', 'help', 'qf', 'fugitive', 'fugitiveblame', 'neo-tree' },
  callback = function(event)
    vim.b[event.buf].ufo_provider = ''
  end,
})

local M = {}

M.autocommands = {
  highlight_yank = {
    {
      event = 'TextYankPost',
      desc = 'Highlight yanked text',
      pattern = '*',
      callback = function()
        vim.hl.on_yank({ timeout = 500 })
      end,
    },
  },

  checktime = {
    {
      event = { 'FocusGained', 'TermClose', 'TermLeave' },
      desc = 'Check if buffers changed on editor focus',
      command = 'checktime',
    },
  },

  create_dir = {
    {
      event = 'BufWritePre',
      desc = "Automatically create parent directories if they don't exist when saving a file",
      callback = function(args)
        if not vim.api.nvim_buf_is_valid(args.buf) then
          return
        end
        vim.fn.mkdir(vim.fn.fnamemodify(vim.uv.fs_realpath(args.match) or args.match, ':p:h'), 'p')
      end,
    },
  },

  q_close_windows = {
    {
      event = 'BufWinEnter',
      desc = 'Make q close help, man, quickfix, dap floats',
      callback = function(event)
        if vim.tbl_contains({ 'help', 'nofile', 'quickfix' }, vim.bo[event.buf].buftype) then
          vim.keymap.set('n', 'q', '<Cmd>close<CR>', {
            desc = 'Close window',
            buffer = event.buf,
            silent = true,
            nowait = true,
          })
        end
      end,
    },
  },

  terminal_settings = {
    {
      event = 'TermOpen',
      desc = 'Disable line number/fold column/sign column for terminals',
      callback = function()
        vim.opt_local.number = false
        vim.opt_local.relativenumber = false
        vim.opt_local.foldcolumn = '0'
        vim.opt_local.signcolumn = 'no'
        vim.opt_local.scrolloff = 0
      end,
    },
  },

  quickfix_tweaks = {
    {
      event = 'FileType',
      desc = 'Unlist quickfist buffers',
      pattern = 'qf',
      callback = function()
        vim.opt_local.buflisted = false
        vim.opt_local.scrolloff = 0
      end,
    },

    {
      event = 'QuickFixCmdPost',
      desc = 'Open quickfix window after running commands',
      pattern = { 'helpgrep', 'grep' },
      command = 'cwindow',
    },

    {
      event = 'FileType',
      pattern = 'qf',
      desc = 'Disable mini.cursorword in quickfix buffers',
      callback = function(args)
        vim.b[args.buf].minicursorword_disable = true
      end,
    }
  },

  sql_fun = {
    {
      event = { 'BufNewFile', 'BufRead' },
      desc = 'Set the filetype for psqlrc',
      pattern = { 'psqlrc', '.psqlrc' },
      callback = function(args)
        vim.b[args.buf].sql_type_override = 'psql'
        vim.bo[args.buf].filetype = 'sql'
      end,
    },
  },

  gitlab_ci_extras = {
    {
      event = { 'BufNewFile', 'BufReadPost', 'FileReadPost' },
      desc = 'Additional gitlab CI files in subdirectories',
      pattern = { '*.ci*.yaml', '*.ci*.yml', '*-ci*.yaml', '*-ci*.yml' },
      callback = function(args)
        local util = require 'lspconfig.util'
        if util.root_pattern('.gitlab-ci.y*ml')(vim.fn.fnamemodify(args.match, ':p:h')) then
          vim.opt_local.filetype = 'yaml.gitlab'
        end
        -- check if in a root with .gitlab-ci.yaml/yml
        -- then set filetype to 'yaml.gitlab'
        -- set the yaml schema?
      end,
    },
  },

  zoxide = {
    {
      event = 'DirChanged',
      desc = 'Add the directory to the zoxide database',
      callback = function(event)
        vim.fn.system({ "zoxide", "add", event.file })
      end
    }
  }
}

function M.create_au()
  for augroup, autocmds in pairs(M.autocommands) do
    if autocmds then
      local augroup_id = vim.api.nvim_create_augroup(augroup, { clear = true })
      for _, autocmd in ipairs(autocmds) do
        local event = autocmd.event
        autocmd.event = nil
        autocmd.group = augroup_id
        autocmd.id = vim.api.nvim_create_autocmd(event, autocmd)
        autocmd.event = event
      end
    end
  end
end

return M
