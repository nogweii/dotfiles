require('nvim-biscuits').setup({
  default_config = {
    max_length = 15,
    min_distance = (vim.o.lines - 3) / 2,
    prefix_string = "🍪"
  },

  -- Per-language overrides
  --[[ language_config = {
    html = {
      prefix_string = " 🌐 "
    },
    javascript = {
      prefix_string = " ✨ ",
      max_length = 80
    },
    python = {
      disabled = true
    }
  } ]]

  -- on_events = { 'InsertLeave', 'CursorHoldI' }

})
