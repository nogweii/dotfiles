require('nvim-treesitter.configs').setup {
  ensure_installed = {
    -- general languages
    "ruby", "python", "lua", "perl", "php", "rust", "java", "kotlin", "haskell",
    -- C family of languages
    "c", "c_sharp", "cpp", "zig", "vala",
    -- tool specific languages
    "cmake", "dockerfile", "todotxt", "make", "dot", "vim",
    -- nix shells
    "bash", "fish",
    -- config markups
    "yaml", "json", "hjson", "json5", "jsonc", "hocon", "hcl", "toml",
    -- web dev (front end)
    "css", "scss", "javascript", "typescript", "jsdoc", "html",
    -- protocols
    "proto", "http", "graphql",
    -- elixir support
    "elixir", "eex", "erlang",
    -- text formatting
    "comment", "markdown", "rst", "regex",
    -- golang & tooling
    "go", "gomod", "gowork",
    -- godot engine
    "gdscript", "godot_resource"
  },

  -- Enable some modules shipped with nvim-treesitter
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true
  },

  -- Colorful brackets to help visualize nest depth
  -- (provided by p00f/nvim-ts-rainbow)
  rainbow = {
    enable = true
  },

  -- Smarter 'commentstring' in files with multiple languages at once (like HTML)
  -- (provided by JoosepAlviste/nvim-ts-context-commentstring)
  context_commentstring = {
    enable = true
  },
}

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
