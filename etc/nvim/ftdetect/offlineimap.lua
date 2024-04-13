vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = { '.offlineimaprc', '*/offlineimap/config', '*/offlineimap/*.conf', '*/offlineimap/*.conf.minimal' },
  callback = function(_au_details)
    vim.opt.filetype = 'offlineimap'
  end,
})
