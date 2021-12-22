-- I like pylint over pyflakes, so disable pyflakes and instead use pylint
-- Also disable pycodestyle since I use black to format the code
-- TODO: read from project-specific config instead of setting this globally
return {
  settings = {
    pylsp = {
      plugins = {
        pyflakes = {
          enabled = false
        },
        pycodestyle = {
          enabled = false
        },

        pylint = {
          enabled = true
        },
      }
    }
  }
}

-- Also note: my preferences require the installation of additional plugins to pylsp:
-- run `:PylspInstall pylsp-mypy pyls-isort python-lsp-black pylsp-rope` in a neovim
