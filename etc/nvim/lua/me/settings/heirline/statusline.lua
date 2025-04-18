local conditions = require("heirline.conditions")
local parts = require("me.settings.heirline.parts")

local DefaultStatusLine = {
  -- Left-hand side of the statusbar
  parts.ViMode, parts.Space, parts.GitSigns, parts.Align,

  -- Middle of the statusbar
  parts.FileNameBlock, parts.Align,

  -- And the right-hand side of the statusbar
  parts.FileEncoding, parts.FileFormat, parts.LSPActive, parts.Diagnostics, parts.Ruler
}

local InactiveStatusline = {
  condition = conditions.is_not_active,
  parts.Align,
  {
    init = function(self)
      self.filename = vim.api.nvim_buf_get_name(0)
    end,
    parts.FileName,
  },
  parts.Align,
}

local HelpFileName = {
  condition = function()
    return vim.bo.filetype == "help"
  end,
  provider = function()
    local filename = vim.api.nvim_buf_get_name(0)
    return vim.fn.fnamemodify(filename, ":t")
  end,
}

local SpecialStatusline = {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "nofile", "prompt", "help", "quickfix" },
      filetype = { "fzf", "^git.*", "fugitive", "fugitiveblame" },
    })
  end,
  parts.Align,
  parts.FileType,
  parts.Space,
  HelpFileName,
  parts.Align,
}

local TerminalStatusline = {
  condition = function()
    return conditions.buffer_matches({ buftype = { "terminal" } })
  end,

  { condition = conditions.is_active, parts.ViMode, parts.Space },
  parts.Align,

  {
    provider = function()
      local tname, _ = vim.api.nvim_buf_get_name(0):gsub(".*:", "")
      return " " .. tname
    end,
    hl = { bold = true },
  },
  parts.Align,
}


return {
  hl = function()
    if conditions.is_active() then
      return "StatusLine"
    else
      return "StatusLineNC"
    end
  end,

  -- Once a condition matches, immediately use that
  fallthrough = false,

  SpecialStatusline,
  TerminalStatusline,
  InactiveStatusline,
  DefaultStatusLine,
}
