local cmp = require('cmp')
local luasnip = require('luasnip')

local lsp_kind_emojis = {
  Text = "📜",
  Method = "🧶",
  Function = "🧵",
  Constructor = "🚧",
  Field = "🏷️",
  Variable = "🔻",
  Class = "📦",
  Interface = "🧩",
  Module = "🚛",
  Property = "💊",
  Unit = "🗳 ",
  Value = "🧪",
  Enum = "🧫",
  Keyword = "🔑",
  Snippet = "🌱",
  Color = "🎨",
  File = "🗄 ",
  Reference = "🪝",
  Folder = "📁",
  EnumMember = "🦠",
  Constant = "🧊",
  Struct = "🧱",
  Event = "🌩️",
  Operator = "❎",
  TypeParameter = "🅾️"
}

local is_prior_char_whitespace = function()
  local col = vim.fn.col('.') - 1
  if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
    return true
  else
    return false
  end
end

local config = {
  -- integrate the snippets engine with cmp
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },

  -- define various completion related keybindings here so that cmp's key
  -- handling doesn't get confused, along with it's cooperation with LSP
  mapping = cmp.mapping.preset.insert {
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      elseif is_prior_char_whitespace() then
        fallback()
      elseif not is_prior_char_whitespace() then
        cmp.complete()
      end
    end, { "i", "s", }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s", }),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace, -- TODO: or do I want Insert?
      select = true,
    },
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
  },

  -- define which sources to enable by default
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'buffer' },
    { name = 'path' },
  }),

  -- customize how the completion menu appears
  formatting = {
    format = function(entry, vim_item)
      -- vim_item is a menu completion item, see |complete-items| for details

      if entry.source.name == "nvim_lsp" then
        vim_item.kind = string.format("%s %s", lsp_kind_emojis[vim_item.kind], vim_item.kind)
      elseif entry.source.name == "luasnip" then
        vim_item.kind = string.format("%s", lsp_kind_emojis["Snippet"])
      elseif entry.source.name == "buffer" then
        vim_item.kind = ""
      end

      vim_item.menu = ({
        nvim_lsp = "[LSP]",
        buffer = "[Buf]",
        luasnip = "[Snip]",
      })[entry.source.name]

      return vim_item
    end,
  },
}

cmp.setup(config)

-- Set configuration for specific filetypes:
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'buffer' },
    { name = 'path' },
  }),
})

-- Use buffer source for `/` (relies on cmp's custom completion UI).
cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})
