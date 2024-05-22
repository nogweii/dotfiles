-- Improving the terraform / OpenTofu editing experience.
-- Also a few other hashicorp tools.

local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'terraform-ls', 'tflint', 'tfsec' })
  }
}
