local lsp_settings = {
  settings = {
    -- See https://ansible.readthedocs.io/projects/vscode-ansible/als/settings/ for a reference
    ansible = {
      completion = {
        provideRedirectModules = false,
        provideModuleOptionAliases = false,
      },
      ansible = {
        useFullyQualifiedCollectionNames = true,
      }
    },
  },
}

return lsp_settings
