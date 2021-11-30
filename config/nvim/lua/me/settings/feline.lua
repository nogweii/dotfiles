require('feline').setup()

local components = {
  -- these components are shown on the focused window
  active = {
    left = {},
    mid = {},
    right = {}
  },

  -- and windows that are not focused, or specific "sidebars".
  -- these get a simpler struture, thus the lack of a "mid"
  inactive = {
    left = {},
    right = {}
  }
}

table.insert(components.active.left, {
})
