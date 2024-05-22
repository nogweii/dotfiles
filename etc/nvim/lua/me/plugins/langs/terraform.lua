-- Improving the terraform / OpenTofu editing experience.
-- Also a few other hashicorp tools.

local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'terraform-ls', 'tflint', 'tfsec' })
  },

  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      {
        "ANGkeith/telescope-terraform-doc.nvim",
        config = function()
          require("telescope").load_extension("terraform_doc")
        end,
      },
      {
        "cappyzawa/telescope-terraform.nvim",
        config = function()
          require("telescope").load_extension("terraform")
        end,
      },
    },
  }
}
