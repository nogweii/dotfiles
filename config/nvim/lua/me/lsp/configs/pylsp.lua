-- I like pylint over pyflakes, so disable pyflakes and instead use pylint
-- Also disable pycodestyle since I use black to format the code
-- TODO: read from project-specific config instead of setting this globally
local is_venv = vim.env.VIRTUAL_ENV ~= nil and vim.env.VIRTUAL_ENV ~= ''

local python_path = vim.fn.exepath('python3')

local lsp_settings = {
  settings = {
    pylsp = {
      plugins = {
        jedi = {
          environment = python_path
        },
        pyflakes = {
          enabled = false
        },
        pycodestyle = {
          enabled = false
        },

        pylint = {
          enabled = true
        },

        pyls_mypy = {
          enabled = true,
          dmypy = true,
          overrides = {"--python-executable", python_path, true}
        }
      }
    }
  }
}

if is_venv then

  -- Check if pylint is installed inside the virtualenv
  local pylint_path = vim.fn.exepath('pylint')
  if vim.startswith(pylint_path, vim.env.VIRTUAL_ENV) then
    -- it is, so rely on launching it as a subprocess rather than the copy of pylint that lives in the LSP's code
    -- (this way, that subprocess pylint can scan all of the packages installed inside)
    lsp_settings['settings']['pylsp']['plugins']['pylint']['executable'] = pylint_path
  end
end

return lsp_settings
-- Also note: my preferences require the installation of additional plugins to pylsp:
-- run `:PylspInstall pylsp-mypy pyls-isort python-lsp-black pylsp-rope` in a neovim
