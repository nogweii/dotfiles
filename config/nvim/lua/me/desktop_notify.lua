-- Doesn't work if we don't have notify-send installed, so fall back to
-- the built-in implementation
if vim.fn.executable("notify-send") == 0 then
  return
end

-- A list of log level codes into human labels
-- See also, :help vim.lsp.set_log_level()
local log_levels = {
    [0] = 'trace',
    [1] = 'debug',
    [2] = 'info',
    [3] = 'warn',
    [4] = 'error',
}

-- When an LSP (or other nvim_notify client) wants to tell me something, use
-- the desktop notification system rather than a message inside neovim
-- See also, :help nvim_notify()
function vim.notify(msg, log_level, opts)
    vim.fn.jobstart{
        'notify-send',
        '--icon', 'nvim',
        '--category', ('x-neovim.log.%s'):format(log_levels[log_level] or 'trace'),
        '--app-name', 'Neovim',
        msg
    }
end
