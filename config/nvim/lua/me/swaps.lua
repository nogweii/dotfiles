-- Add additional cycle groups for monaqa's dial plugin to make it behave
-- similarly to SwapIt, toggle.vim, and/or cycle.vim

local dial = require("dial")

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

for enum_name, string_list in pairs(addtional_enums) do
  dial.augends["me#" .. enum_name] = dial.common.enum_cyclic{
      name = enum_name,
      strlist = string_list,
  }
  table.insert(dial.config.searchlist.normal, "me#" .. enum_name)
end
