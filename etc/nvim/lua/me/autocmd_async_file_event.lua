-- Create a custom autocommand that only fires when a real file is loaded.
-- This is utilized by my lazy.nvim configuration to delay loading file-specific
-- plugins until a file is actually loaded.
--
-- This is comparable to LazyNvim's "LazyFile" and AstroNvim's "AstroFile" events.
-- (And, in fact, is based on their respective code.)

-- Tell lazy.nvim about my magical special event that fires when a proper file is loaded.
-- The functions registered to the event will be ran asynchronously, out of the main UI loop.
local event = require('lazy.core.handler.event')
event.mappings.AsyncFileLoad = { id = 'AsyncFileLoad', event = 'User', pattern = 'AsyncFileLoad' }
event.mappings['User AsyncFileLoad'] = event.mappings.AsyncFileLoad

local group_name = 'special_plugin_events'

vim.api.nvim_create_autocmd({ 'BufReadPost', 'BufNewFile', 'BufWritePre' }, {
  group = vim.api.nvim_create_augroup(group_name, { clear = true }),
  desc = 'A file has loaded, asynchronously trigger more plugins',
  nested = true,
  callback = function(args)
    vim.schedule(function()
      -- skip any invalid buffers
      if not vim.api.nvim_buf_is_loaded(args.buf) then
        return
      end

      local filename = vim.api.nvim_buf_get_name(args.buf)
      if not (filename == '' or vim.bo[args.buf].buftype == 'nofile') then
        -- This looks like a real file, send the signal!
        vim.schedule(function()
          vim.api.nvim_exec_autocmds('User', {
            pattern = 'AsyncFileLoad',
            modeline = false,
          })
        end)

        -- Now that the custom event has been fired, delete this callback from
        -- memory so it doesn't fire *again*. (Like ++once but smarter)
        vim.api.nvim_del_autocmd(args.id)

        -- The plugins that were newly loaded almost certainly set their own
        -- file-specific autocommands, so make sure they are told about the event.
        vim.schedule(function()
          vim.api.nvim_exec_autocmds(args.event, { buffer = args.buf, data = args.data })
        end)
      end
    end)
  end,
})
