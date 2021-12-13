local wk = require 'which-key'
wk.setup {
}

wk.register({
  g = {
    name = 'Git',
    y = 'Yank a URL to the git repo',
    p = 'Show the diff of the hunk',
    S = 'Stage the hunk',
    U = 'Unstage the hunk',
    b = 'View history of current line',
  },
  ['hi'] = 'Show the highlight group of the current word',
  ['ml'] = 'Append a vim modeline to the end of the file',
  d = {
    name = "Diagnostics",
    L = 'Toggle diagnostics display',
    l = 'Manually run linters',
    d = 'Show full linter message in a popup',
    f = 'Run all fixers on the file',
  }
}, { prefix = '<leader>' })

wk.register({
  L = 'Edit the neovim config',
  P = 'Edit the snippets file for this file type',
  W = 'Save changes',
  D = 'Close the buffer',
  E = 'Fuzzily find a file',
  G = 'Search for text in files',
  U = 'Visualize undo tree',
  T = 'Search for code tags',
}, { prefix = 'Z' })
