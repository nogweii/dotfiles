require('compe').setup {
  enabled = true,
  autocomplete = true, -- automatically open the completion menu
  min_length = 3, -- minimum number of characters before completing items
  debug = false,

  source = {

    -- common sources
    path = true,
    buffer = true,
    tags = true,
    spell = true,
    calc = false,
    omni = false,
    emoji = true,

    -- neovim specific sources
    nvim_lsp = true,
    nvim_lua = true,

    -- plugin sources
    vim_lsp = false,
    vim_lsc = false,
    vsnip = false,
    ultisnips = true,
    snippets_nvim = false,
    nvim_treesitter = true,
  };
}
