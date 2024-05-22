-- Command to invoke rbenv to launch the correct ruby version and get details.
-- Copied from https://github.com/Shopify/ruby-lsp/blob/main/vscode/src/ruby/rbenv.ts
local rbenv_command = [[
rbenv exec ruby -W0 -rjson -e 'STDERR.print({env: ENV.to_h,yjit:!!defined?(RubyVM::YJIT),version:RUBY_VERSION}.to_json)'
]]

local setup_options = {}

setup_options["options"] = {
  rubyLsp = {
    -- rubyVersionManager = {
    --   identifier = "custom",
    --   customRubyCommand = rbenv_command
    -- }
  },
}

return setup_options
