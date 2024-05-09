local helpers = require("null-ls.helpers")

return helpers.make_builtin({
  name = "typos",
  meta = {
    url = "https://github.com/crate-ci/typos",
    description = "Finds and corrects spelling mistakes among source code with low false positives.",
  },
  method = require("null-ls.methods").internal.DIAGNOSTICS,
  filetypes = {},
  generator_opts = {
    command = "typos",
    format = "line",
    to_stdin = true,
    args = { "--format=brief", "-" },
    check_exit_code = function(c)
      return (c == 0) or (c == 2)
    end,
    on_output = helpers.diagnostics.from_patterns({
      {
        pattern = "-:(%d+):(%d+): (.*)",
        groups = { "row", "col", "message" },
      }
    }),
  },
  factory = helpers.generator_factory,
})
