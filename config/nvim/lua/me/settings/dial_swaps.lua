-- Add additional cycle groups for monaqa's dial.nvim

local augend = require("dial.augend")

-- Start with the ones built-in to the plugin
local my_augends = {
  augend.semver.alias.semver,
  augend.integer.alias.decimal_int,
  augend.integer.alias.hex,
  augend.integer.alias.octal,
  augend.integer.alias.binary,
  augend.date.alias["%m/%d"],
  augend.date.alias["%Y/%m/%d"],
  augend.date.alias["%Y-%m-%d"],
  augend.date.alias["%H:%M:%S"],
  augend.date.alias["%H:%M"],
  augend.hexcolor.new{
    case = "lower",
  },
  augend.hexcolor.new{
    case = "upper",
  }
}

-- NB: these enumerated lists are active in every buffer, they aren't scoped to
-- a particular file type... this could be unwanted behavior!
local addtional_enums = {
  boolean = {"true", "false"},
  capitalBoolean = {"True", "False"},
  c_bools = {"TRUE", "FALSE"},
  booleanPunctuation = {"&&", "||"},

  onoff = {"on", "off"},
  yesno = {"yes", "no"},

  c_define = {"define", "undef"},

  html_headings = {"h1", "h2", "h3", "h4", "h5", "h6"},

  rebase = {"pick", "reword", "edit", "squash", "fixup", "exec"},
  directions = {"up", "down", "left", "right"},
  cardinals = {"north", "east", "south", "west"},
}

-- Then add a whole bunch of additional ones based on static lists of words
for _, string_list in pairs(addtional_enums) do
  table.insert(my_augends, augend.constant.new{
    elements = string_list,
    word = true,
    cyclic = true,
  })
end

require("dial.config").augends:register_group{
  default = my_augends
}
