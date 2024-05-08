local helpers = require('null-ls.helpers')

return helpers.make_builtin({
  name = 'formatjson5',
  meta = {
    url = 'https://github.com/google/json5format/',
    description = 'JSON5 formatter that preserves contextual comments ',
  },
  method = require('null-ls.methods').internal.FORMATTING,
  filetypes = { 'json5' },
  generator_opts = {
    command = 'formatjson5',
    args = { '--one_element_lines', '--sort_arrays', '-' },
    to_stdin = true,
  },
  factory = helpers.formatter_factory,
})
