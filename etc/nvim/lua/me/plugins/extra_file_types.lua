---@type LazySpec
return {
  { 'Joorem/vim-haproxy', ft = 'haproxy' },

  {
    'NoahTheDuke/vim-just',
    ft = { 'just' },
  },

  {
    'fladson/vim-kitty',
    ft = { 'kitty', 'kitty-session' },
  },

  -- {
  --   'towolf/vim-helm',
  --   ft = 'helm'
  -- },

  {
    url = 'https://gitlab.com/HiPhish/jinja.vim.git',
    name = 'jinja.vim',
    submodules = false,
  }
}
