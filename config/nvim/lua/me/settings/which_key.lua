local wk = require 'which-key'
wk.setup { }

wk.register({
  g = {
    name = 'Git',
    y = 'Yank a URL to this file in a git repo',
    p = 'Show the diff of the hunk',
    S = 'Stage the hunk',
    U = 'Unstage the hunk',
    b = 'View history of current line',
    Y = 'Yank homepage of the git repo',
    B = 'Open the git repo in a browser',
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
  D = 'Close the buffer',
  E = 'Fuzzily find a file',
  G = 'Search for text in files',
  L = 'Edit the neovim config',
  P = 'Edit the snippets file for this file type',
  T = 'Search for code tags',
  R = 'Show all LSP diagnostics',
  U = 'Visualize undo tree',
  W = 'Save changes',
}, { prefix = 'Z' })
