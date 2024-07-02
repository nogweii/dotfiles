-- Various additional filetype definitions
vim.filetype.add({
  extension = {
    hujson = 'json5',
    -- jinja2 = 'jinja',
    -- j2 = 'jinja',
    -- jinja = 'jinja',
  },

  pattern = {
    ['.*/zsh/functions/.*']   = 'zsh',
    ['.*/zsh/zle-widgets/.*'] = 'zsh',

    -- Gitlab CI yaml detection, as the normal filename or as .gitlab/ci/*
    ['%.gitlab%-ci%.yml']     = 'yaml.gitlab',
    ['%.gitlab%-ci%.yaml']    = 'yaml.gitlab',
    ['%.gitlab/ci/.*%.yml']   = 'yaml.gitlab',
    ['%.gitlab/ci/.*%.yaml']  = 'yaml.gitlab',

    ['.*/%.kube/config']           = 'yaml',
  },

  filename = {
    ['.codespellrc'] = 'ini',
    ['.sqlfluff'] = 'cfg',
    Podfile = 'ruby',
    Fastfile = 'ruby',
    Guardfile = 'ruby',
    ['.gemrc'] = 'yaml',
    gemrc = 'yaml',
    ['.yamlfmt'] = 'yaml',
    ['.ecrc'] = 'json',
  },
})
