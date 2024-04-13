local lsp_settings = {
  settings = {
    -- See https://als.readthedocs.io/en/latest/settings/ for a reference
    ansible = {
      completion = {
        provideRedirectModules = false,
        provideModuleOptionAliases = false,
      },
    },
  },
}

return lsp_settings
