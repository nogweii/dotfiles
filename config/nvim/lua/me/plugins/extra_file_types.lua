return {
  -- Support HCL and other Hashicorp specific syntaxes
  { 'hashivim/vim-terraform' },

  -- addtional syntax highlighting for postgresql extensions
  {
    'lifepillar/pgsql.vim',
    ft = 'sql',
  },

  -- Add TICKscript (Influx Kapacitor 1.x) syntax
  { 'nathanielc/vim-tickscript' },

  -- ReasonML & ReScript syntax support
  { 'amiralies/vim-reason' },
  { 'rescript-lang/vim-rescript' },

  { 'Joorem/vim-haproxy' },

  {
    'vim-crystal/vim-crystal',
    init = function()
      -- Disable the key mappings the plugin would make, most of them are redundant since I use
      -- specific plugins to handle the (generic) functionality
      vim.g.crystal_define_mappings = 0
      -- I'll handle integrating the various tools with the other purpose-focused plugins
      vim.g.crystal_auto_format = 0
      -- cmp & the LSPs handle this for me, don't need the basic completion this plugin provides
      vim.g.crystal_enable_completion = 0
    end,
  },

  {
    'NoahTheDuke/vim-just',
    ft = { 'just' },
  },

  {
    'fladson/vim-kitty',
  },
}
