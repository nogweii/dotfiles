-- Bootstrap lazy.nvim by automatically cloning the git repo
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  if not vim.fn.executable("git") then
    print("Lazy.nvim not installed and git not found in PATH. Plugins aren't available!")
  else
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable", -- latest stable release
      lazypath,
    })
    print("Lazy.nvim git repository cloned.")
  end
end
vim.opt.rtp:prepend(lazypath)

-- Unicode has variants of the same glyph in multiple tables. Early pictographic symbols are an example
-- of them being duplicated in the emoji list. (think: b&w smiley face vs full-color version.) Appending
-- this codepoint will cause the previous character to always be selected from the emoji table.
local force_emoji = '\u{FE0F}'

---@type LazyConfig
local opts = {
  -- Replace some of the icons with emoji
  ui = {
    icons = {
      cmd = "⌘" .. force_emoji,
      config = "🛠" .. force_emoji,
      event = "📅",
      ft = "📂",
      init = "⚙" .. force_emoji,
      keys = "🗝" .. force_emoji,
      plugin = "🔌",
      runtime = "💻" .. force_emoji,
      source = "📄",
      start = "🚀",
      task = "📌",
      lazy = "💤 ",
    },
  },

  defaults = {
    version = "*",
  },
  performance = {
    rtp = {
      paths = { "/usr/lib/nvim" },
      disabled_plugins = {
        "gzip",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      }
    },
  },
}

require("lazy").setup("me.plugins", opts)
