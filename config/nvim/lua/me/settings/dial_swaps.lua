-- Add additional cycle groups for monaqa's dial plugin to make it behave
-- similarly to SwapIt, toggle.vim, and/or cycle.vim

local augend = require("dial.augend")

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

local built_in_augends = {
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
  augend.constant.alias.alpha,
  augend.constant.alias.Alpha,
}

local cyclic_augends = {}
for enum_name, string_list in pairs(addtional_enums) do
  table.insert(cyclic_augends, augend.constant.new{
    elements = string_list,
    word = false,
    cyclic = true,
  })
end

local extra_augends = {
  augend.hexcolor.new{
    case = "lower",
  },
  augend.hexcolor.new{
    case = "upper",
  }
}

require("dial.config").augends:register_group{
  default = vim.tbl_extend("keep", built_in_augends, extra_augends, cyclic_augends),
}
