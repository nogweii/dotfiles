---@type LazySpec
return {
  {
    'milanglacier/minuet-ai.nvim',
    config = function()
      require('minuet').setup {
        after_cursor_filter_length = 20,
        provider = 'openai_fim_compatible',
        provider_options = {
          openai_fim_compatible = {
            model = 'qwen2.5-coder:7b',
            end_point = 'https://ollama.ai.aether.earth/v1/completions',
            name = 'Ollama',
            stream = true,
            api_key = 'HOME', -- this needs to be a non-null environment variable, but Ollama doesn't care
            optional = {
              stop = nil,
              max_tokens = nil,
            }
          }
        },
        notify = 'warn',
      }
    end,
  },
}
