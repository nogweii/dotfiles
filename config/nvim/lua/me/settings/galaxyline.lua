local gl = require("galaxyline")
local gls = gl.section
local condition = require('galaxyline.condition')
local colors = require('zephyr')

gl.short_line_list = {'NvimTree','vista','dbui','packer'}

local active_bg = colors.base2
local inactive_bg = colors.base1

-- vim's mode() function returns character codes that aren't printable for
-- some modes. get those character codes to fill in the mode map
local lit_ctrl_v = vim.api.nvim_replace_termcodes('<C-v>', true, true, true)
local lit_ctrl_s = vim.api.nvim_replace_termcodes('<C-s>', true, true, true)
local mode_map = {
  ['n']    = {'NORMAL', colors.black},
  ['no']   = {'OP-PEND', colors.grey},
  ['nov']  = {'OP-PEND', colors.grey},
  ['noV']  = {'OP-PEND', colors.grey},
  ['niI']  = {'NORMAL', colors.black},
  ['niR']  = {'NORMAL', colors.black},
  ['niV']  = {'NORMAL', colors.black},
  ['v']    = {'VISUAL', colors.violet},
  ['V']    = {'VIS-LINE', colors.violet},
  ['s']    = {'SELECT', colors.magenta},
  ['S']    = {'SEL-LINE', colors.magenta},
  ['i']    = {'INSERT', colors.light_green},
  ['ic']   = {'INSERT', colors.light_green},
  ['ix']   = {'INSERT', colors.light_green},
  ['R']    = {'REPLACE', colors.redwine},
  ['Rc']   = {'REPLACE', colors.redwine},
  ['Rv']   = {'VIS-REPL', colors.violet},
  ['Rx']   = {'REPLACE', colors.redwine},
  ['c']    = {'COMMAND', colors.teal},
  ['cv']   = {'EX', colors.teal},
  ['ce']   = {'EX', colors.teal},
  ['r']    = {'REPLACE', colors.redwine},
  ['rm']   = {'MORE', colors.dark_green},
  ['r?']   = {'CONFIRM', colors.dark_green},
  ['!']    = {'SHELL', colors.dark_green},
  ['t']    = {'TERMINAL', colors.dark_green},

  -- A few special ones that are annoying to represent in lua
  ['no' .. lit_ctrl_v] = {'OP-PEND', colors.grey},
  [lit_ctrl_v]         = {'VIS-BLOCK', colors.violet},
  [lit_ctrl_s]         = {'SEL-BLOCK', colors.magenta},
}

gls.left[1] = {
  VimMode = {
    provider = function()
      local text = mode_map[vim.fn.mode()][1]
      local color = mode_map[vim.fn.mode()][2]
      vim.cmd('hi GalaxyVimMode guibg=' .. color)
      return text..' '
    end,
    highlight = { colors.fg_alt, active_bg },
    separator = ' ',
    separator_highlight = { colors.fg, colors.brown },
  }
}
gls.left[2] = {
  LineAndColumn = {
    provider = function()
      local line = vim.fn.line('.')

      -- this gets the "literal byte column", which counts tabs a 1 character
      -- long, even though visually they are 4 (or 8!) columns long
      -- local column = vim.fn.col('.')

      -- this is the cursor's position on screen, which means the underlying
      -- byte length in line of text is not used.
      local column = vim.fn.virtcol('.')

      local line_padded = line .. string.rep(' ', 3 - #tostring(line))
      local column_padded = column .. string.rep(' ', 2 - #tostring(column))

      return ''.. line_padded .. ' ' .. column_padded
    end,
    highlight = { colors.black, colors.brown },
  }
}

gls.left[3] = {
  AleErrorCount = {
    provider = function()
      local ale_counts = vim.fn["ale#statusline#Count"](vim.fn.bufnr())
      return ale_counts.error
    end,
    highlight = 'LspDiagnosticsSignError',
    icon = ' ' .. vim.g.ale_sign_error .. ' ',
    condition = function()
      local ale_counts = vim.fn["ale#statusline#Count"](vim.fn.bufnr())
      return ale_counts.error > 0
    end,
  }
}
gls.left[4] = {
  AleWarningCount = {
    provider = function()
      local ale_counts = vim.fn["ale#statusline#Count"](vim.fn.bufnr())
      return ale_counts.warning
    end,
    highlight = 'LspDiagnosticsSignWarning',
    icon = ' ' .. vim.g.ale_sign_warning .. ' ',
    condition = function()
      local ale_counts = vim.fn["ale#statusline#Count"](vim.fn.bufnr())
      return ale_counts.warning > 0
    end,
  }
}
gls.left[5] = {
  AleInfoCount = {
    provider = function()
      local ale_counts = vim.fn["ale#statusline#Count"](vim.fn.bufnr())
      return ale_counts.info
    end,
    highlight = 'LspDiagnosticsSignInformation',
    icon = ' ' .. vim.g.ale_sign_info .. ' ',
    condition = function()
      local ale_counts = vim.fn["ale#statusline#Count"](vim.fn.bufnr())
      return ale_counts.info > 0
    end,
  }
}

gls.mid[1] = {
  FileIcon = {
    provider = 'FileIcon',
    condition = condition.buffer_not_empty,
    highlight = { require('galaxyline.provider_fileinfo').get_file_icon_color, active_bg },
  }
}
gls.mid[2] = {
  FileName = {
    provider = function()
      return vim.fn.expand("%")
    end,
    condition = condition.buffer_not_empty,
    highlight = { colors.base8, active_bg },
  }
}
gls.mid[3] = {
  ModifiedIcon = {
    provider = function()
      if vim.bo.modifiable and vim.bo.modified then
        return '💾'
      end
    end,
    highlight = { colors.magenta, active_bg },
  }
}

gls.right[1] = {
  LspIsActive = {
    provider = function()
      local icon = '力'
      local icon_gone = '年' -- TODO: do I show this when lspinstall/lspconfig knows about a LSP that isn't installed?
      if next(vim.lsp.get_active_clients()) == nil then
        -- no lsp connected
        icon = ''
      end
      return icon
    end,
    highlight = { colors.teal, active_bg }
  }
}
gls.right[2] = {
  SpellCheck = {
    provider = function()
      local color = colors.teal
      local gui_style = "NONE"
      if not vim.wo.spell then
	color = colors.redwine
        gui_style = "strikethrough"
      end
      vim.cmd('hi GalaxySpellCheck guifg=' .. color .. ' gui=' .. gui_style .. ' guibg=' .. active_bg)
      return '暈'
    end
  }
}
gls.right[3] = {
  GitBranch = {
    provider = 'GitBranch',
    condition = condition.check_git_workspace,
    highlight = { colors.base1, colors.orange },
    icon = " "
  }
}
gls.right[4] = {
  FileFormat = {
    provider = function()
      local icons = {
        ["mac"] = '',
        ["dos"] = '',
        ["unix"] = '',
      }
      local icon = icons[vim.bo.fileformat]
      return icon .. ' ' .. vim.bo.fileformat
    end,
    condition = function()
      if vim.bo.fileformat == "unix" then
        return false
      else
        return true
      end
    end,
    highlight = { colors.base1, colors.magenta },
  }
}


gls.short_line_left[1] = {
  BufferType = {
    provider = 'BufferIcon',
    separator = ' ',
    separator_highlight = {colors.grey, inactive_bg},
    highlight = {colors.grey, inactive_bg}
  }
}
gls.short_line_left[2] = {
  SFileName = {
    provider =  'FileName',
    condition = condition.buffer_not_empty,
    highlight = {colors.grey, inactive_bg}
  }
}

-- Reload galaxyline after ALE finishes so that the counts update
vim.cmd('autocmd User ALELintPost lua require("galaxyline").load_galaxyline()')
