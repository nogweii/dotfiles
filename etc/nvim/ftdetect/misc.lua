-- Various additional filetype definitions
vim.filetype.add({
  extension = {
    hujson = 'json5',
  },

  pattern = {
    ['.*/zsh/functions/.*'] = 'zsh',
    ['.*/zsh/zle-widgets/.*'] = 'zsh',
  },

  filename = {
    ['.codespellrc'] = 'ini',
    ['.sqlfluff'] = 'cfg',
    Podfile = 'ruby',
    Fastfile = 'ruby',
    ['.gemrc'] = 'yaml',
    gemrc = 'yaml',
    ['.yamlfmt'] = 'yaml',
    ['.ecrc'] = 'json',
  },
})
