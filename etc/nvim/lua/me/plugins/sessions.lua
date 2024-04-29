local M = {}

-- Patterns to ignore when saving sessions
M.ignores = {
  -- directories to not create sessions for
  dirs = {
    '~',
    '$XDG_CACHE_HOME',
    '$XDG_CONFIG_HOME',
    '$XDG_DATA_HOME',
    '$XDG_RUNTIME_DIR',
    '$XDG_STATE_HOME',
  },
  -- filetypes to exclude from sessions
  filetypes = {
    'gitcommit',
    'gitrebase',
  },
  -- buffer types (see :help 'buftypes') that aren't included in the session
  buftypes = {
    'help',
  },
}

--- Check if a buffer is one that should be included in sessions
---@param bufnr integer The buffer to check
---@return boolean # Whether the buffer is one we care to include
function M.does_resession_care(bufnr)
  if not bufnr then
    bufnr = 0
  end
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return false
  end
  -- If the buffer is marked hidden, for any reason, skip over it
  if vim.bo[bufnr].bufhidden ~= '' then
    return false
  end

  local buftype = vim.bo[bufnr].buftype
  local filetype = vim.bo[bufnr].filetype

  if buftype == '' then
    -- Normal buffer, but unlisted? Skip.
    if not vim.bo[bufnr].buflisted then
      return false
    end
    -- No filename? Skip!
    if vim.api.nvim_buf_get_name(bufnr) == '' then
      return false
    end
  end

  if vim.tbl_contains(M.ignores.filetypes, filetype) or vim.tbl_contains(M.ignores.buftypes, buftype) then
    return false
  end
  return true
end

local group = vim.api.nvim_create_augroup('dotfiles/SessionManager', { clear = true })
-- Autocommand 1: When launching neovim with no arguments, restore the directory's session
vim.api.nvim_create_autocmd('VimEnter', {
  group = group,
  callback = function()
    local cwd = vim.fn.getcwd()
    -- Check if we're in a directory that I don't want sessions for
    for _, dir in ipairs(M.ignores.dirs) do
      if vim.fn.expand(dir) == cwd then
        -- yup, one of the expanded results matches, so bail out early
        return false
      end
    end

    local resession = require('resession')
    -- Only load the session if nvim was started with no args
    if vim.fn.argc(-1) == 0 then
      resession.load(cwd, { dir = 'dirsession', silence_errors = true })
    end
  end,
  nested = true,
})

-- Autocommand 2: When quitting neovim, save the session if there is anything to save
vim.api.nvim_create_autocmd('VimLeavePre', {
  group = group,
  callback = function()
    local cwd = vim.fn.getcwd()
    -- Check if we're considering saving a per-directory session in a directory
    -- I want to exclude
    for _, dir in ipairs(M.ignores.dirs) do
      if vim.fn.expand(dir) == cwd then
        -- yup, one of the expanded results matches, so bail out early
        return false
      end
    end

    -- run through the list of buffers, seeing if we care about any of 'em
    local buffer_cares = vim.tbl_map(M.does_resession_care, vim.api.nvim_list_bufs())
    if vim.tbl_contains(buffer_cares, true) then
      -- there is at least one! save the session. possibly including others we
      -- don't care about, but those will be skipped on load because buf_filters
      local resession = require('resession')
      resession.save(vim.fn.getcwd(), { dir = 'dirsession', notify = false })
    end
  end,
})

---@type LazySpec
return {
  {
    'stevearc/resession.nvim',
    opts = {
      -- as resession is iterating through the buffers it saved, this
      -- function will filter out any that should be ignored
      buf_filters = function(bufnr)
        M.does_resession_care(bufnr)
      end,
    },
  },
}
