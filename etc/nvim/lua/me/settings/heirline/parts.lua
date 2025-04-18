local conditions = require("heirline.conditions")
local utils = require("heirline.utils")
local M = {}

M.Space = { provider = " " }

M.Align = { provider = "%=" }

M.ViMode = {
  -- get vim current mode, this information will be required by the provider
  -- and the highlight functions, so we compute it only once per component
  -- evaluation and store it as a component attribute
  init = function(self)
    self.mode = vim.fn.mode(1) -- :h mode()

    -- Maximum possible length for a mode name (used for padding)
    self.mode_name_max_length = math.max(unpack(vim.tbl_map(function(str)
      return #str
    end, vim.tbl_values(self.mode_label))))
  end,

  -- Now we define some dictionaries to map the output of mode() to the
  -- corresponding string and color. We can put these into `static` to compute
  -- them at initialisation time.
  static = {
    mode_label = {
      n         = "NORMAL",
      no        = "OP",
      nov       = "OP",
      noV       = "OP",
      ["no\22"] = "OP",
      niI       = "NORMAL",
      niR       = "NORMAL",
      niV       = "NORMAL",
      nt        = "NORMAL",
      ntT       = "NORMAL",
      v         = "VISUAL",
      vs        = "VISUAL",
      V         = "LINES",
      Vs        = "LINES",
      ["\22"]   = "BLOCK",
      ["\22s"]  = "BLOCK",
      s         = "SELECT",
      S         = "SELECT",
      ["\19"]   = "SELECT",
      i         = "INSERT",
      ic        = "INSERT",
      ix        = "INSERT",
      R         = "REPLACE",
      Rc        = "REPLACE",
      Rx        = "REPLACE",
      Rv        = "REPLACE",
      Rvc       = "REPLACE",
      Rvx       = "REPLACE",
      c         = "COMMAND",
      cr        = "COMMAND",
      cv        = "COMMAND",
      cvr       = "COMMAND",
      r         = "ENTER",
      rm        = "MORE",
      ["r?"]    = "CONFIRM",
      ["!"]     = "EXTERNAL",
      t         = "TERMINAL",
    },

    vi_mode_bg = {
      BLOCK    = "blue",
      COMMAND  = "green",
      CONFIRM  = "cyan",
      ENTER    = "cyan",
      EXTERNAL = "cyan",
      INSERT   = "red",
      LINES    = "blue",
      MORE     = "cyan",
      NORMAL   = "green",
      OP       = "green",
      REPLACE  = "purple",
      SELECT   = "purple",
      TERMINAL = "black",
      VISUAL   = "blue",
    }
  },

  -- We can now access the value of mode() that, by now, would have been
  -- computed by `init()` and use it to index our strings dictionary.
  -- note how `static` fields become just regular attributes once the
  -- component is instantiated.
  provider = function(self)
    local mode_name = self.mode_label[self.mode]
    local padding_length = self.mode_name_max_length - #mode_name

    -- Center-align the mode label
    return string.rep(' ', math.ceil(padding_length / 2))
        .. mode_name
        .. string.rep(' ', math.floor(padding_length / 2))
  end,

  hl = function(self)
    return { fg = "vim_mode_fg", bg = "vim_mode_" .. string.lower(self.mode_label[self.mode]), }
  end,

  -- Re-evaluate the component only on ModeChanged event!
  -- Also allows the statusline to be re-evaluated when entering operator-pending mode
  update = {
    "ModeChanged",
    pattern = "*:*",
    callback = vim.schedule_wrap(function()
      vim.cmd("redrawstatus")
    end),
  },
}

M.Ruler = {
  -- %l = current line number
  -- %c = column number
  -- %P = percentage through file of displayed window
  provider = "%7(%l:%c%) %P",
  hl = { bg = "ruler_bg" }
}

M.LSPActive = {
  condition = conditions.lsp_attached,
  update = { 'LspAttach', 'LspDetach' },

  provider = "󰒋 ",

  on_click = {
    callback = function()
      vim.defer_fn(function()
        vim.cmd("LspInfo")
      end, 100)
    end,
    name = "heirline_LSP",
  },

  -- hl = { fg = "green", bold = true },
}

M.Diagnostics = {
  update = { "LspAttach", "DiagnosticChanged", "BufEnter" },

  static = {
    error_icon = '',
    warn_icon = '',
    info_icon = '',
    hint_icon = '',
  },
  condition = function()
    return #vim.diagnostic.get(0) > 0
  end,
  init = function(self)
    self.errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
    self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
    self.hints = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
    self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
  end,

  on_click = {
    callback = function()
      require("trouble").toggle({ mode = "document_diagnostics" })
      -- or
      -- vim.diagnostic.setqflist()
    end,
    name = "heirline_diagnostics",
  },

  { -- Errors
    provider = function(self)
      return self.errors > 0 and self.error_icon .. " " .. self.errors
    end,
    hl = { fg = utils.get_highlight("DiagnosticSignError").fg },
  },
  { -- Warnings
    provider = function(self)
      return self.warnings > 0 and self.warn_icon .. " " .. self.warnings
    end,
    hl = { fg = utils.get_highlight("DiagnosticSignWarn").fg },
  },
  { -- Infos
    provider = function(self)
      return self.info > 0 and self.info_icon .. " " .. self.info
    end,
    hl = { fg = utils.get_highlight("DiagnosticSignInfo").fg },
  },
  { -- Hints
    provider = function(self)
      return self.hints > 0 and self.hint_icon .. " " .. self.hints
    end,
    hl = { fg = utils.get_highlight("DiagnosticSignHint").fg },
  }
}

M.GitSigns = {
  condition = conditions.is_git_repo,

  init = function(self)
    self.status_dict = vim.b.gitsigns_status_dict
    self.has_changes = self.status_dict.added ~= 0 or self.status_dict.removed ~= 0 or self.status_dict.changed ~= 0
  end,

  hl = { bg = "git_bg" },

  { -- git branch name
    provider = function(self)
      return " " .. self.status_dict.head
    end,
  },
  {
    condition = function(self)
      return self.has_changes
    end,
    provider = "("
  },
  {
    provider = function(self)
      local count = self.status_dict.added or 0
      return count > 0 and ("+" .. count)
    end,
    hl = { fg = "git_add" },
  },
  {
    provider = function(self)
      local count = self.status_dict.removed or 0
      return count > 0 and ("-" .. count)
    end,
    hl = { fg = "git_del" },
  },
  {
    provider = function(self)
      local count = self.status_dict.changed or 0
      return count > 0 and ("~" .. count)
    end,
    hl = { fg = "git_change" },
  },
  {
    condition = function(self)
      return self.has_changes
    end,
    provider = ")",
  },
}

M.FileIcon = {
  init = function(self)
    local filename = vim.api.nvim_buf_get_name(0)
    local extension = vim.fn.fnamemodify(filename, ":e")
    self.icon, self.icon_color = require("nvim-web-devicons").get_icon_color(filename, extension, { default = true })
  end,
  provider = function(self)
    return self.icon and (self.icon .. " ")
  end,
  hl = function(self)
    return { fg = self.icon_color }
  end
}

M.FileName = {
  provider = function(_self)
    -- first, trim the pattern relative to the current directory
    local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":.")
    if filename == "" then return "[No Name]" end

    -- now, if the filename would occupy more than 1/4th of the available
    -- space, trim the file path to its initials
    if not conditions.width_percent_below(#filename, 0.25) then
      filename = vim.fn.pathshorten(filename)
    end
    return filename
  end,
  hl = { fg = utils.get_highlight("Directory").fg },
}

M.FileFlags = {
  {
    condition = function()
      return vim.bo.modified
    end,
    provider = "💾",
  },
  {
    condition = function()
      return not vim.bo.modifiable or vim.bo.readonly
    end,
    provider = "🔒",
  },
}

M.FileNameBlock = {
  M.FileIcon,
  M.FileFlags,
  M.FileName,
  { provider = '%<' }, -- this means that the statusline is cut here when there's not enough space
  hl = { bg = "file_bg" },
}

M.FileType = {
  provider = function()
    return string.upper(vim.bo.filetype)
  end,
  hl = { fg = utils.get_highlight("Type").fg },
}

M.FileEncoding = {
  provider = function()
    local enc = (vim.bo.fenc ~= '' and vim.bo.fenc) or vim.o.enc -- :h 'enc'
    return enc ~= 'utf-8' and enc:upper()
  end
}

M.FileFormat = {
  provider = function()
    local fmt = vim.bo.fileformat
    return fmt ~= 'unix' and fmt:upper()
  end
}

return M
